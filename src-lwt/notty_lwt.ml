(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Lwt.Infix

open Notty
open Notty_unix
open Private


type ('a, 'b) either = Left of 'a | Right of 'b
let left  x = Left x
let right y = Right y

let (</>) a b = Lwt.pick [(a >|= left); (b >|= right)]
let (<??>) a b = (a >|= left) <?> (b >|= right)

let whenopt f = function Some x -> f x | None -> ()

let rec write fd buf off = function
  | 0 -> Lwt.return_unit
  | n -> Lwt_unix.write fd buf off n >>= fun w -> write fd buf (off + w) (n - w)

module Lwt_condition = struct

  include Lwt_condition

  let map f c =
    let d = create () in
    let rec go () = wait c >>= fun x -> broadcast d (f x); go ()
    in (Lwt.async go; d)

  let unburst ~t c =
    let d = create () in
    let rec delay x = Lwt_unix.sleep t </> wait c >>= function
      | Left () -> broadcast d x; start ()
      | Right x -> delay x
    and start () = wait c >>= delay in
    Lwt.async start; d
end

module Term = struct

  let winches = lazy (
    let c = Lwt_condition.create () in
    let `Revert _ = set_winch_handler (Lwt_condition.broadcast c) in
    c
  )

  let winch () = Lazy.force winches |> Lwt_condition.wait

  let bsize = 1024

  let input_stream ~nosig fd stop =
    let `Revert f = setup_tcattr ~nosig (Lwt_unix.unix_file_descr fd) in
    let stream =
      let flt  = Unescape.create ()
      and ibuf = Bytes.create bsize in
      let rec next () =
        match Unescape.next flt with
        | #Unescape.event as r -> Lwt.return_some r
        | `End   -> Lwt.return_none
        | `Await ->
            (Lwt_unix.read fd ibuf 0 bsize <??> stop) >>= function
              | Left n  -> Unescape.input flt ibuf 0 n; next ()
              | Right _ -> Lwt.return_none
      in Lwt_stream.from next in
    Lwt.async (fun () -> Lwt_stream.closed stream >|= f);
    stream

  type t = {
    ochan  : Lwt_io.output_channel
  ; trm    : Tmachine.t
  ; buf    : Buffer.t
  ; fds    : Lwt_unix.file_descr * Lwt_unix.file_descr
  ; events : [ Unescape.event | `Resize of (int * int) ] Lwt_stream.t
  ; stop   : (unit -> unit)
  }

  let write t =
    Tmachine.output t.trm t.buf;
    let out = Buffer.contents t.buf in (* XXX There goes 0copy. :/ *)
    Buffer.clear t.buf; Lwt_io.write t.ochan out

  let refresh t      = Tmachine.refresh t.trm; write t
  let image t image  = Tmachine.image t.trm image; write t
  let cursor t curs  = Tmachine.cursor t.trm curs; write t
  let set_size t dim = Tmachine.set_size t.trm dim
  let size t         = Tmachine.size t.trm

  let release t =
    if Tmachine.release t.trm then
      ( t.stop (); write t >>= fun () -> Lwt_io.flush t.ochan )
    else Lwt.return_unit

  let resizef fd stop on_resize =
    if Unix.isatty fd then
      let rcond = Lwt_condition.(
        Lazy.force winches |> unburst ~t:0.1 |> map (fun () -> winsize fd)) in
      let rec monitor () =
        (Lwt_condition.wait rcond <?> stop) >>= function
          | Some dim -> on_resize dim; monitor ()
          | None     -> Lwt.return_unit in
      Lwt.async monitor;
      Lwt_stream.from (fun () -> Lwt_condition.wait rcond <?> stop)
        |> Lwt_stream.map (fun dim -> `Resize dim)
    else Lwt_stream.of_list []

  let create ?(dispose=true) ?(nosig=true) ?(mouse=true) ?(bpaste=true)
             ?(input=Lwt_unix.stdin) ?(output=Lwt_unix.stdout) () =
    let fd = Lwt_unix.unix_file_descr output in
    let (stop, stopw) = Lwt.wait () in
    let rec t = lazy {
        trm    = Tmachine.create ~mouse ~bpaste (cap_for_fd fd)
      ; ochan  = Lwt_io.(of_fd ~mode:output) output
      ; buf    = Buffer.create 4096
      ; fds    = (input, output)
      ; stop   = (fun () -> Lwt.wakeup stopw None)
      ; events = Lwt_stream.choose
          [ input_stream ~nosig input stop
          ; resizef fd stop @@ fun dim ->
              let t = Lazy.force t in Buffer.reset t.buf; set_size t dim ]
      } in
    let t = Lazy.force t in
    winsize fd |> whenopt (set_size t);
    Lwt.async (fun () -> write t); (* XXX async? *)
    if dispose then
      Lwt_sequence.add_r (fun () -> release t)
        Lwt_main.exit_hooks [@warning "-3"] |> ignore;
    t

  let events t = t.events
  let fds    t = t.fds
end

let winsize fd = winsize (Lwt_unix.unix_file_descr fd)

include Gen_output (struct
  type fd = Lwt_unix.file_descr and k = unit Lwt.t
  let (def, to_fd) = Lwt_unix.(stdout, unix_file_descr)
  and write fd buf = Buffer.(write fd (to_bytes buf) 0 (length buf))
end)
