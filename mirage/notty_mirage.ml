open Notty
open Lwt.Infix

(* XXX General, common up, possibly Mirage-Types. *)
module type SFLOW = sig

  type +'a io

  type input
  type output
  type flow
  type error

  val error_message : error -> string
  val read   : flow -> [`Ok of input | `Eof | `Error of error ] io
  val write  : flow -> output -> [`Ok of unit | `Eof | `Error of error ] io
  val writev : flow -> output list -> [`Ok of unit | `Eof | `Error of error ] io
  val close  : flow -> unit io

end

module type SFLOW_LWT = SFLOW with type 'a io = 'a Lwt.t
(* /XXX *)

(** XXX This should die and go to Cstruct. *)
module Cstruct = struct
  include Cstruct
  let to_bytes cs = Bytes.unsafe_of_string (to_string cs)
end
(* /XXX *)

(* Seriously? It's 2016 and this isn't in the standard library?? *)
let rec fmap f = function
  | []    -> []
  | x::xs -> match f x with Some y -> y :: fmap f xs | _ -> fmap f xs

let ok x = Lwt.return (`Ok x)

let (>>==) a fb = a >>= function
  | `Ok x -> fb x
  | `Error _ | `Eof as e -> Lwt.return e


module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

module Term (L : TERMINAL_LINK) = struct

  type 'a io = 'a Lwt.t

  type flow = {
    flow : L.flow
  ; trm  : Tmachine.t
  ; iflt : Unescape.t
  }

  type error = L.error
  let error_message = L.error_message

  type input  = [ Unescape.event | `Resize of (int * int) ]
  type output = [ `Image of image | `Cursor of (int * int) option ]

  let output t =
    match Tmachine.output t.trm with
    | `Output o -> L.write t.flow (`Data (Cstruct.of_string o))
    | `Await    -> ok () (* Flush? *)

  let set_size t dim = Tmachine.set_size t.trm dim

  let writev t msgs =
    msgs |> List.iter (function
      | `Cursor curs -> Tmachine.cursor t.trm curs
      | `Image img   -> Tmachine.image t.trm img);
    output t

  let write t msg = writev t [msg]

  let rec read t =
    match Unescape.next t.iflt with
    | #Unescape.event as e -> ok e
    | `End   -> Lwt.return `Eof
    | `Await ->
        L.read t.flow >>= function
          | `Error _ | `Eof as e   -> Lwt.return e
          | `Ok (`Resize dim as r) -> set_size t dim; ok r
          | `Ok (`Data buf) ->
              Unescape.input t.iflt (Cstruct.to_bytes buf) 0 (Cstruct.len buf);
              read t

  let create ?(mouse=true) ?(cap=Cap.ansi) flow =
    let t = {
        trm  = Tmachine.create ~mouse cap
      ; flow = flow
      ; iflt = Unescape.create ()
    } in
    set_size t (80, 24);
    L.write flow (`Line_edit false) >>== fun () -> output t >>== fun () -> ok t

  let close t =
    if Tmachine.release t.trm then
      output t >>= fun _ -> L.close t.flow
    else Lwt.return_unit

end

module Terminal_link_of_console (C : V1_LWT.CONSOLE) = struct
  type 'a io = 'a Lwt.t
  type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
  type output = [ `Data of Cstruct.t | `Line_edit of bool ]
  type flow  = C.t
  type error = C.error
  let error_message = C.error_message
  let close = C.close
  let writev t xs =
    fmap (function `Data x -> Some x | _ -> None) xs |> C.writev t
  let write t = function
    | `Data buf -> C.write t buf
    | _         -> ok ()
  let read t = C.read t >|= function
    | `Ok buf -> `Ok (`Data buf)
    | `Error _ | `Eof as e -> e
end
