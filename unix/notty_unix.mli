(** [Notty] IO for pure [Unix].

    This is an IO module for {!Notty}. Consult its {{!Notty}documentation} for
    the basics.

    {b Note} All {!Notty.image} output requires {{!Notty.Cap.t}terminal
    capabilities}. These are usually auto-detected, by checking that the output
    is a tty, that the environment variable [$TERM] is set, and that it is not
    set to either [""] or ["dumb"]. If these conditions hold,
    {{!Notty.Cap.ansi}ANSI} escapes are used. Otherwise, {{!Notty.Cap.dumb}no}
    escapes are used.

    *)
open Notty

(** {1:fullscreen Fullscreen input and output}. *)

(** Terminal IO abstraction for fullscreen, interactive applications.

    This module provides both input and output. It assumes exlusive ownership of
    the IO streams between {{!create}initialization} and {{!release}shutdown}.
    *)
module Terminal : sig

  type t
  (** Representation of the terminal, giving structured access to IO. *)

  (** {1 Construction and destruction} *)

  val create : ?dispose:bool ->
               ?mouse:bool ->
               ?input:Unix.file_descr ->
               ?output:Unix.file_descr ->
               unit -> t
  (** [create ~dispose ~mouse ~input ~output ()] creates a fresh {{!t}terminal}.

      [create] has the following side effects:
      {ul
      {- [Unix.tcsetattr] is applied to [input] to disable {e echo} and {e
         canonical mode}.}
      {- [output] is set to {e alternate screen mode}, the cursor is hidden, and
         mouse reporting is enabled, using the appropriate control sequences.}
      {- [SIGWINCH] signal, normally ignored, is handled. From then on, resizing
         the window will interrupt blocking syscalls with the {!Unix.Unix_error}
         [EINTR] exception.}}

      [~dispose] arranges for automatic {{!release}cleanup} of the terminal
      before the process terminates. The downside is that a reference to this
      terminal is retained until the program exits. Defaults to [true].

      [~mouse] activates mouse reporting. Defaults to [true].

      [~input] is the input file descriptor. Defaults to [stdin].

      [~output] is the output file descriptor. Defaults to [stdout]. *)

  val release : t -> unit
  (** Dispose of this terminal. Original behavior of input is reinstated,
      cursor is restored, mouse reporting disabled, and alternate mode is
      terminated.

      It is an error to use the {{!cmds}commands} on a released terminal.
      [release] itself is idempotent. *)

  (** {1:cmds Commands} *)

  val image : t -> image -> unit
  (** Sets a new {{!Notty.image}[image]} and redraws the terminal. *)

  val refresh : t -> unit
  (** Redraws the terminal. Useful if the output might have become garbled. *)

  val cursor : t -> (int * int) option -> unit
  (** Sets and redraws the cursor.

      [None] hides it. [Some (x, y)] places it at column [x] and row [y], with
      the origin at [(1, 1)], mapping to the upper-left corner. *)

  (** {1 Input} *)

  val input : t -> [ Unescape.event | `Resize of (int * int) | `End ]
  (** Wait for new input.

      [input t] can be:
      {ul
      {- [#Unescape.event] with {!Notty.Unescape.event} from the input fd;}
      {- [`End] if the input fd is closed, or the terminal was released; or}
      {- [`Resize (cols * rows)] giving the current size of the output tty, if a
         [SIGWINCH] was delivered before or during this call to [input].}}

      {b Note} [input] is buffered. Calls to [input] can either block or
      immediately return. Use {!input_pending} to detect when the next call
      would not block. *)

  val input_pending : t -> bool
  (** [input_pending t] is [true] if the next call to [input] would not block
      and the terminal has not yet been released *)

  (** {1 Properties} *)

  val size : t -> (int * int)
  (** [size t] is the current size of the terminal's output tty. *)

  (** {1 Window size change notifications} *)

  (** Manual [SIGWINCH] handling.

      Unix delivers notifications about tty size changes through the [SIGWINCH]
      signal. A handler for this signal is installed as soon as a new terminal
      is {{!create}created}. Replacing the global [SIGWINCH] handler using
      the [Sys] module will cause this module to malfunction, as the size change
      notifications will no longer be delivered.

      You might still want to ignore resizes reported by {!input} and directly
      listen to [SIGWINCH]. This module allows installing such listeners
      without conflicting with the rest of the machinery. *)
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
        handler has already been removed. *)
  end
end

(** {1:inline Inline output}

    These operations do not change the terminal state and do not assume
    exclusive access to the output. They can be combined with other means of
    producing output. *)

val winsize : Unix.file_descr -> (int * int) option
(** [winsize fd] is [Some (columns, rows)], the current dimensions of [fd]'s
    backing tty, or [None], when [fd] is not backed by a tty. *)

val output_image_f : ?cap:Cap.t -> out_channel -> (int * int -> image) -> unit
(** [output_image ~cap channel f] writes the image returned by [f] to [channel].

    [f] is applied to the current dimensions of [channel]'s backing tty. When
    [channel] is not backed by a tty, this is [(80, 24)].

    The resulting image is displayed in its full height. If the output is a tty,
    image width is clipped to the output width, otherwise, full width is used.

    [cap] is the optional terminal capability set.

    {b Note} No leading or trailing characters are produced, so:
    {ul
    {- an image 1-cell high can be a part of a line of output, preceded and/or
       followed by other output;}
    {- if the cursor is on on the first column, image of any height can be
       correctly printed; and}
    {- outputting an image higher than 1 when the cursor has advanced
       past the first colum will result in mis-aligned output.}}

    {b Note} Default value for [f] when the output is not a tty is mere
    convenience. Use {!Unix.isatty} or {!winsize} to detect whether the output
    has a well-defined size. *)

val output_image : ?cap:Cap.t -> out_channel -> image -> unit
(** [output_image ~cap channel i] writes the image [i] to [channel]. *)

val print_image_f : (int * int -> image) -> unit
(** [print_image_f f] is [output_image_f stdout f]. *)

val print_image : image -> unit
(** [print_image i] is [output_image stdout i]. *)

val print_image_nl : image -> unit
(** [print_image_nl i] is [print_image i], followed by a newline. *)


(**/**)

(** {1 Private interfaces}

    These are subject to change and shouldn't be used. *)
module Private : sig

  val cap_for_fd         : Unix.file_descr -> Cap.t
  val setup_tcattr       : Unix.file_descr -> [ `Revert of (unit -> unit) ]
  val set_winch_handler  : (unit -> unit) -> [ `Revert of (unit -> unit) ]
  val output_image_f_gen : to_fd:('fd -> Unix.file_descr) ->
                           write:('fd -> Buffer.t -> 'r) ->
                           ?cap:Cap.t -> 'fd -> (int * int -> image) -> 'r
end
(**/**)
