open Lwt

open Notty
open Notty_unix

let whenopt f = function Some x -> f x | None -> ()

let output_buffer fd buf =
  let rec go b off = function
    | 0 -> return_unit
    | n -> Lwt_unix.write fd b off n >>= fun w -> go b (off + w) (n - w) in
  go (Buffer.contents buf) 0 (Buffer.length buf)

let (</>) a b = pick [(a >|= fun x -> `Left x); (b >|= fun x -> `Right x)]

module Lwt_condition = struct

  include Lwt_condition

  let fmap f c =
    let d = create () in
    let rec go () = wait c >>= fun x -> f x |> whenopt (broadcast d); go ()
    in (async go; d)

  let unburst ~t c =
    let d = create () in
    let rec delay x = Lwt_unix.sleep t </> wait c >>= function
      | `Left () -> broadcast d x; start ()
      | `Right x -> delay x
    and start () = wait c >>= delay in
    async start; d
end

module Terminal = struct

  let winches = lazy (
    let c = Lwt_condition.create () in
    let `Revert _ = set_winch_handler (Lwt_condition.broadcast c) in
    c
  )

  let next_winch () = Lwt_condition.wait (Lazy.force winches)

  let input_stream fd =
    let `Revert f = setup_tcattr (Lwt_unix.unix_file_descr fd) in
    let stream =
      let flt  = Unescape.create ()
      and ibuf = Bytes.create 64 in
      let rec read () =
        match Unescape.next flt with
        | #Unescape.event as r -> return_some r
        | `End   -> return_none
        | `Await ->
            Lwt_unix.read fd ibuf 0 Bytes.(length ibuf) >>= fun n ->
              Unescape.input flt ibuf 0 n; read ()
      in Lwt_stream.from read
    in
    Lwt_stream.on_terminate stream f;
    (stream, f)

  type t = {
    ochan  : Lwt_io.output_channel
  ; trm    : Tmachine.t
  ; input  : Unescape.event Lwt_stream.t * (unit -> unit)
  ; size_c : (int * int) Lwt_condition.t
  }

  let next_resize t = Lwt_condition.wait t.size_c

  let rec write t =
    match Tmachine.output t.trm with
    | `Output s -> Lwt_io.write t.ochan s >>= fun () -> write t
    | `Await    -> Lwt_io.flush t.ochan

  let release t =
    if Tmachine.finish t.trm then
      ( snd t.input (); write t )
    else return_unit

  let redraw t       = Tmachine.refresh t.trm; write t
  let image t image  = Tmachine.image t.trm image; write t
  let cursor t curs  = Tmachine.cursor t.trm curs; write t
  let set_size t dim = Tmachine.set_size t.trm dim

  let resizes fd =
    let fd = Lwt_unix.unix_file_descr fd in
    Lwt_condition.(
      Lazy.force winches |> unburst ~t:0.1 |> fmap (fun () -> winsize fd)
    )

  let winch_monitor autosize t =
    let rec go () =
      next_resize t >|= set_size t >>= fun () ->
        if autosize then redraw t >>= go else go () in
    go

  let create ?(dispose=true) ?(autosize=true) ?(input=Lwt_unix.stdin) ?(output=Lwt_unix.stdout) () =
    let fd = Lwt_unix.unix_file_descr output in
    let t = {
        trm    = Tmachine.create (cap_for_fd fd)
      ; ochan  = Lwt_io.(of_fd ~mode:output) output
      ; input  = input_stream input
      ; size_c = resizes output
      } in
    async (fun () -> winsize fd |> whenopt (set_size t); redraw t);
    async (winch_monitor autosize t);
    if dispose then
      Lwt_sequence.add_r (fun () -> release t) Lwt_main.exit_hooks |> ignore;
    t

  let input t = fst t.input

  let size t = Tmachine.size t.trm

end

let output_image =
  output_image_gen
    ~to_fd:Lwt_unix.unix_file_descr
    ~write:output_buffer

let print_image = output_image ~cap:(cap_for_fd Unix.stdout) Lwt_unix.stdout

let winsize fd = winsize (Lwt_unix.unix_file_descr fd)
