(** [Notty] IO [Lwt] on [Unix].

    This is an IO module for {!Notty}. Consult its {{!Notty}documentation} for
    the basics.

    It mirrors {!Notty_unix} and the corresponding operations behave
    analogously. Consult its {{!Notty_unix}documentation} for more info. *)
open Notty

(** {1:fullscreen Fullscreen input and output}. *)

(** Terminal IO with concurrency.

    For more info, see {!Notty_unix.Terminal}. *)
module Terminal : sig

  type t

  (** {1 Construction and destruction} *)

  val create : ?dispose:bool ->
               ?input:Lwt_unix.file_descr ->
               ?output:Lwt_unix.file_descr ->
               unit -> t
  (** [create ~dispose ~input ~output ()] is a new {{!t}terminal}.

      {b Note} [~dispose] arranges for the terminal to be disposed of at the end
      of [Lwt] main loop, not at process exit. 
      
      See {!Notty_unix.Terminal.create}. *)

  val release : t -> unit Lwt.t

  (** {1 Commands} *)

  val image   : t -> image -> unit Lwt.t
  val refresh : t -> unit Lwt.t
  val cursor  : t -> (int * int) option -> unit Lwt.t

  (** {1 Input} *)

  val events : t -> [ Unescape.event | `Resize of (int * int) ] Lwt_stream.t
  (** [events t] is the stream of incoming events.

      Invoking {!release} will terminate this stream.

      Events are:
      {ul
      {- [#Unescape.event] with {!Notty.Unescape.event} from the input fd; or}
      {- [`Resize (int * int)] whenever the terminal size changes.}}

      {b Note} This stream is unique; for the same [t], [events t] always
      returns the same stream. *)

  (** {1 Properties} *)

  val size : t -> (int * int)

  (** {1 Window size change notifications}

      {{!create}Creating} a terminal will install a [SIGWINCH] handler.
      The handler should not be replaced directly. This API allows the user to
      monitor deliveries of the signal.

      See {!Notty_unix.Terminal.Winch}. *)

  val winch : unit -> unit Lwt.t
  (** [winch ()] is a thread completing after the next [SIGWINCH]. A single
      signal delivery will cause the completion of all waiting [winch] threads. *)
end

(** {1:inline Inline output} *)

val winsize : Lwt_unix.file_descr -> (int * int) option

val output_image_f : ?cap:Cap.t -> Lwt_unix.file_descr -> (int * int -> image) -> unit Lwt.t

val output_image : ?cap:Cap.t -> Lwt_unix.file_descr -> image -> unit Lwt.t

val print_image_f : (int * int -> image) -> unit Lwt.t

val print_image : image -> unit Lwt.t

val print_image_nl : image -> unit Lwt.t
