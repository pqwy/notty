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

(*
 * Ideally, someone working on, say, _telnet_, would map this space out and we
 * would model the input/output types after that.
 *)
module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

open Notty

module Term (F : TERMINAL_LINK) : sig

  include SFLOW_LWT
    with type input  = [ Unescape.event | `Resize of (int * int) ]
     and type output = [ `Image of image | `Cursor of (int * int) option ]

  val create : ?mouse:bool -> ?cap:Notty.Cap.t -> F.flow -> [ `Ok of flow | `Error of error | `Eof ] Lwt.t
end

module Terminal_link_of_console (C : V1_LWT.CONSOLE) :
  TERMINAL_LINK with type flow = C.t
