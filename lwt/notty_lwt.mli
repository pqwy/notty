open Notty

module Terminal : sig

  type t
  type event = [ `Uchar of uchar | `Key of Unescape.key ]

  val create : ?dispose:bool ->
               ?winch:bool ->
               ?input:Lwt_unix.file_descr ->
               ?output:Lwt_unix.file_descr ->
                unit -> t

  val release : t -> unit Lwt.t

  val wait_resize : t -> (int * int) Lwt.t

  val resize  : t -> (int * int) -> unit Lwt.t
  val inputs  : t -> event Lwt_stream.t
  val refresh : t -> unit Lwt.t
  val update  : t -> ?cursor:(int * int) -> image -> unit Lwt.t

  val size    : t -> (int * int)

end
