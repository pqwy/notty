(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** [Notty] IO [Lwt] on [Unix].

    This is an IO module for {!Notty}.

    It mirrors {!Notty_unix} and the corresponding operations behave
    analogously. Consult its documentation for more info.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Notty

(** {1:fullscreen Fullscreen input and output}. *)

(** Terminal IO with concurrency.

    For more info, see {!Notty_unix.Term}. *)
module Term : sig

  type t

  (** {1 Construction and destruction} *)

  val create : ?dispose:bool ->
               ?nosig:bool ->
               ?mouse:bool ->
               ?bpaste:bool ->
               ?input:Lwt_unix.file_descr ->
               ?output:Lwt_unix.file_descr ->
               unit -> t
  (** [create ~dispose ~nosig ~mouse ~input ~output ()] creates a new
      {{!t}terminal}.

      {b Note} [~dispose] arranges for the terminal to be disposed of at the end
      of the [Lwt] main loop, and not at process exit.

      See {!Notty_unix.Term.create}. *)

  val release : t -> unit Lwt.t

  (** {1 Commands} *)

  val image   : t -> image -> unit Lwt.t
  val refresh : t -> unit Lwt.t
  val cursor  : t -> (int * int) option -> unit Lwt.t

  (** {1 Events} *)

  val events : t -> [ Unescape.event | `Resize of (int * int) ] Lwt_stream.t
  (** [events t] is the stream of incoming events.

      Invoking {{!release}release} will terminate this stream.

      Events are:
      {ul
      {- [#Unescape.event], an {{!Notty.Unescape.event}event} from the input
         fd; or}
      {- [`Resize (int * int)] whenever the terminal size changes.}}

      {b Note} This stream is unique; for the same [t], [events t] always
      returns the same stream. *)

  (** {1 Properties} *)

  val size : t -> int * int

  val fds : t -> Lwt_unix.file_descr * Lwt_unix.file_descr

  (** {1 Window size change notifications}

      {{!create}Creating} a terminal will install a [SIGWINCH] handler.
      The handler should not be replaced directly. This API allows the user to
      monitor deliveries of the signal.

      See {!Notty_unix.Term.Winch}. *)

  val winch : unit -> unit Lwt.t
  (** [winch ()] is a thread completing after the next [SIGWINCH]. A single
      signal delivery will cause the completion of all waiting [winch] threads. *)
end

(** {1:inline Inline output} *)

val winsize : Lwt_unix.file_descr -> (int * int) option

val eol : image -> image

val output_image :
  ?cap:Cap.t -> ?fd:Lwt_unix.file_descr -> image -> unit Lwt.t

val output_image_size :
  ?cap:Cap.t -> ?fd:Lwt_unix.file_descr -> (int * int -> image) -> unit Lwt.t

val show_cursor : ?cap:Cap.t -> ?fd:Lwt_unix.file_descr -> bool -> unit Lwt.t

val move_cursor :
  ?cap:Cap.t -> ?fd:Lwt_unix.file_descr ->
    [ `Home | `By of int * int | `To of int * int ] -> unit Lwt.t
