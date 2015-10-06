(** [Notty] IO for [Unix] and [Lwt].

    This is an IO module for {!Notty}. Consult its {{!Notty}documentation} for
    the basics.

    It mirrors {!Notty_unix} and the corresponding operations behave
    analogously. Consult its {{!Notty_unix}documentation} for details.  *)
open Notty

(** Full-screen terminal IO with concurrency. *)
module Terminal : sig

  (** {1 Construction and destruction} *)

  type t

  val create : ?dispose:bool ->
               ?autosize:bool ->
               ?input:Lwt_unix.file_descr ->
               ?output:Lwt_unix.file_descr ->
               unit -> t
  (** [create ~dispose ~autosize ~input ~output ()] is a new {{!t}terminal}.

      For side-effects of this functions and the meaning of its arguments, see
      the {{!Notty_unix.Terminal.create}blocking version}. *)

  val release : t -> unit Lwt.t
  (** [release t] cleans up [t]. See the
      {{!Notty_unix.Terminal.release}blocking version}. *)

  (** {1 Commands} *)

  val image  : t -> image -> unit Lwt.t
  val redraw : t -> unit Lwt.t
  val cursor : t -> (int * int) option -> unit Lwt.t

  (** {1 Input} *)

  val input : t -> [ `Uchar of uchar | `Key of Unescape.key ] Lwt_stream.t
  (** [input t] is the stream of inputs this terminal receives. There is only
      one one such stream, in the sense of [==].

      See {!Notty_unix.Terminal.input} and {!Notty.Unescape.next_k}. *)

  (** {1 Properties} *)

  val size : t -> (int * int)

  (** {1 Window size change notifications} *)

  (** {{!create}Creating} a {{!t}terminal} will install a [SIGWINCH] handler.
      These operations allow the user to monitor deliveries of this signal.

      See {!Notty_unix.Terminal.Winch}. *)

  val next_winch : unit -> unit Lwt.t
  (** [next_winch ()] is a thread completing after the next [SIGWINCH]. *)

  val next_resize : t -> (int * int) Lwt.t
  (** [next_resize t] is a thread completing after the next [SIGWINCH].
      Yields the {{!t}terminal's} output tty size at that moment. *)
end

val output_image : ?cap:Cap.t -> Lwt_unix.file_descr -> image -> unit Lwt.t
(** Outputs the {{!image}image} on the file descriptor. See
   {!Notty_unix.output_image}. *)

val print_image : image -> unit Lwt.t
(** {{!output_image}Output image} to [stdout]. *)

val winsize : Unix.file_descr -> (int * int) option
(** Window size. Same as {!Notty_unix.winsize}. *)
