(** [Notty] IO for pure [Unix].

    This is an IO module for {!Notty}. Consult its {{!Notty}documentation} for
    the basics.
    *)
open Notty

(** Full-screen terminal IO. *)
module Terminal : sig

  type t
  (** An interactive terminal, combining input and output. *)

  (** {1 Construction and destruction} *)

  val create : ?dispose:bool ->
               ?autosize:bool ->
               ?input:Unix.file_descr ->
               ?output:Unix.file_descr ->
               unit -> t
  (** [create ~dispose ~autosize ~input ~output ()] is a fresh {{!t}terminal},
      giving structured access to [input] and [output] suitable for full-screen
      terminal programs.

      [create] has the following side effects:
      {ul
      {- [Unix.tcsetattr] is applied to [input] to disable {e echo} and {e
         canonical mode}.}
      {- [output] is set to {e alternate screen mode} and the cursor is hidden
         using the appropriate escape sequences.}
      {- [SIGWINCH] signal, normally ignored, is handled. This means that
         IO in your program will now be interrupted by [EINTR] on every window
         resize.}}

      [dispose] arranges for automatic {{!release}cleanup} of the terminal
      before the process terminates. The downside is that a reference to this
      terminal is retained until the program exits. Defaults to [true].

      [autosize] activates automatic {{!redraw}redrawing} of output whenever
      the window is resized. Defaults to [true].

      [input] is the input file descriptor. Defaults to [stdin].

      [output] is the output file descriptor. Defaults to [stdout].

      {{!Cap}Capabilities} are detected as in {!output_image}.

      {b Note} It is probably a poor idea to create multiple terminals sharing
      the same [input] or [output]. *)

  val release : t -> unit
  (** Dispose of this terminal. Original behavior of input is reinstated, the
      output is cleared, cursor is restored and alternate mode is terminated.  *)

  (** {1 Commands} *)

  val image : t -> image -> unit
  (** Sets a new {{!Notty.image}[image]} and redraws the terminal. *)

  val redraw : t -> unit
  (** Redraws the terminal. Useful if the output might have become garbled. *)

  val cursor : t -> (int * int) option -> unit
  (** Sets and redraws the cursor.

      [None] hides it. [Some (x, y)] places it at column [x] and row [y], with
      the origin at [(1, 1)], mapping to the upper-left corner. *)

  (** {1 Input} *)

  val input : t -> [ Unescape.event | `End ]
  (** Wait for new input {!Notty.Unescape.event} to arrive.

      If [t] has {{!create}[autosize]}, [input] will silently restart and mask
      any [SIGWINCH] delivered while the call is in progress. *)

  (** {1 Properties} *)

  val size : t -> (int * int)
  (** [size t] is the current size of the terminal's output tty. *)

  (** {1 Window size change notifications} *)

  (** Manual [SIGWINCH] handling.

      Unix delivers notifications about tty size changes through the [SIGWINCH]
      signal. A handler for this signal is installed as soon as a new terminal
      is {{!create}created}. Replacing the global [SIGWINCH] handler using
      the [Sys] module will cause this module to malfunction, as size change
      notifications will no longer be delivered.

      You might still wish to intercept this signal, however. If you are using
      more complex event handling, you might want to disable
      {{!create}[autosize]} and manually schedule {{!redraw}terminal updates}.

      This module allows listening to [SIGWINCH] without conflicting with the
      rest of the machinery. *)
  module Winch : sig

    type remove
    (** Removal token. *)

    val add : Unix.file_descr -> ((int * int) -> unit) -> remove
    (** [add fd f] registers a [SIGWINCH] handler. Every time the signal is
        delivered, [f] is called with the current size of the tty backing [fd].
        If [fd] is not a tty, [f] is never called.

        Handlers are called in the order of their registration. *)

    val remove : remove -> unit
    (** [remove r] removes the handler associated with [r]. Does nothing if the
        handler was already removed. *)
  end
end

val output_image : ?cap:Cap.t -> out_channel -> image -> unit
(** [output_image ~cap channel i] writes the image [i] to [channel].

    Height of the output is the height of [i], while the width is width of the
    tty [channel] is backed by, or width of [i] if this is not the case.

    Note that no leading or trailing characters are produced.
    This means that:
    {ul
    {- an image 1-cell high can be a part of a line of text, preceded and/or
       followed by any other output;}
    {- if the cursor is on on the first column, image of any height can be
       printed; and}
    {- simply outputting an image higher than 1 when the cursor has advanced
       past colum 1 will result in broken graphics. }}

    Auto-detection for [cap] merely checks that both the environment variable
    [$TERM] is set, and that the file descriptor backing [channel]
    [Unix.isatty]. If both are true, {{!Cap.ansi}ANSI} escapes are used.
    Otherwise, {{!Cap.dumb}no} escapes are used. *)

val print_image : image -> unit
(** [output_image stdout] *)

val print_endline : image -> unit
(** Like [print_image], followed by a newline. *)

val winsize : Unix.file_descr -> (int * int) option
(** [winsize fd] is [Some (w, h)] in character cells if [Unix.isatty fd],
    and [None] otherwise. *)

(**/**)

(** {2 Private interfaces}
    These are subject to change. *)
val cap_for_fd        : Unix.file_descr -> Cap.t
val setup_tcattr      : Unix.file_descr -> [ `Revert of (unit -> unit) ]
val set_winch_handler : (unit -> unit) -> [ `Revert of (unit -> unit) ]
val output_image_gen  : to_fd:('fd -> Unix.file_descr) ->
                        write:('fd -> Buffer.t -> 'r) ->
                        ?cap:Cap.t -> 'fd -> image -> 'r
(**/**)
