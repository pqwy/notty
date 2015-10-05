
(** Declaring terminals.

    Notty is a terminal library that revolves around construction and
    composition of displayable {{!I}images}.

    It provides simple image {{!print_image}output}, a {{!Terminal} terminal}
    abstraction with input and output, and standalone image {{!Render}rendering}
    and escape sequence {{!Unescape}parsing}.

    {b Note} [Notty] assumes that the terminal is using UTF-8 for input and
    output. Things might break arbitrarily if this is not the case.

    {b Note} [Notty] does not use terminfo. If your terminal is particularly
    idiosyncratic, things might fail to work. Get in touch with the author to
    expand support. *)

(** {1 Interface} *)

type uchar = int
(** A lone unicode code point. *)

type attr
(** Visual characteristics of displayed text. *)

type image
(** The core data type of things that can be output to a terminal. *)

(** [A] is for attribute.

    Construction and composition of visual characteristics of text. *)
module A : sig

  (** {1 Colors}

      {b Note} Presently, no attempt is made to remap colors depending on the
      terminal. This means that the colors not recognized by a particular
      terminal will simply be ignored in the output. *)

  type color
  (** One of the 256 terminal colors.

      The first 16 are supported on most terminals. Their names are
      standardized, but the actual colors are not. In addition, these colors
      are user definable on many terminal emulators.

      The next 216 form the 6*6*6 color cube.

      The final 24 are the grayscale ramp.  *)

  (** {2 Core 16 colors} *)

  val black        : color
  val red          : color
  val green        : color
  val yellow       : color
  val blue         : color
  val magenta      : color
  val cyan         : color
  val lightgray    : color
  val darkgray     : color
  val lightred     : color
  val lightgreen   : color
  val lightyellow  : color
  val lightblue    : color
  val lightmagenta : color
  val lightcyan    : color
  val white        : color

  (** {2 Extended colors} *)

  val rgb : r:int -> g:int -> b:int -> color
  (** [rgb ~r ~g ~b] is an extended color from the RGB cube indexed by its
      three components. All three components must be in the range [0-5].

      @raise Invalid_argument if a component is outside the range. *)

  val gray : level:int -> color
  (** [gray ~level] is a color on the grayscale ramp. [level] must be in the
      range [0-23].

      @raise Invalid_argument if the [level] is outside the range. *)

  (** {1 Text styles} *)

  type style
  (** Additional text properties. *)

  val bold      : style
  val italic    : style
  val underline : style
  val blink     : style
  val reverse   : style

  (** {1 Attribute composition} *)

  (** {{!attr}Attribute} describes visual appearance of fragments of text.

      They combine a foreground and a background {{!color}color} with a set of
      {{!style}styles}. Either color can be {e missing} from an attribute, in
      which case the terminal's {e default} foreground (resp. background) is
      used.

      Attributes are used to construct primitive {{!I}images}. *)

  val empty : attr
  (** [empty] is the attribute with no foreground or background color and with
      an empty style set. *)

  val (&) : attr -> attr -> attr
  (** [a1 & a2] is the attribute that has [a2]'s foreground (resp. background)
      if not {e missing}, or [a1]'s otherwise, and the union of both style sets.

      {{!attr}[attr]}, {{!empty}[empty]} and {{!(&)}[&]} form a monoid. *)

  val (@/) : color -> attr -> attr
  (** [c @/ a] is [a] with [c] for its foreground. *)

  val (@//) : color -> attr -> attr
  (** [c @// a] is [a] with [c] for its background. *)

  val (@+) : style -> attr -> attr
  (** [s @+ a] is [a] with [s] added to the set of its styles. *)

  val fg : color -> attr
  (** [fg c] is [empty] with [c] for its foreground. *)

  val bg : color -> attr
  (** [bg c] is [empty] with [c] for its background. *)

  val st : style -> attr
  (** [st s] is [empty] with [s] as its single style. *)

  val equal : attr -> attr -> bool
end

(** [I] is for image.

    Construction and composition of images. *)
module I : sig

  (** {1 The meaning of images}

      {!image} is a rectangle of styled character cells. It has a width and
      height, but is not anchored to an origin. A single character with
      associated display attributes, or a short fragment of text, are simple
      images.

      Images can be combined by placing them {{!(<|>)}beside} each other,
      {{!(<->)}above} each other, or {{!(<^>)}over} each other. Using these
      operators, it is possible to start with simple images and build up to
      arbitrarily complex patterns.

      Once constructed, image can be rendered and only at that point it obtains
      absolute placement.

      {b Note.} Text fragments in images generally must not contain control
      characters. These are taken to be code points in the interval
      [0x00 - 0x1f], together with [0x7f]. *)

  (** {1 Image properties} *)

  val height : image -> int
  val width  : image -> int

  (** {1 Primitive constructors} *)

  val empty  : image
  (** [empty] is a zero-sized image. *)

  val string : attr -> string -> image
  (** [string attr string] is an image consisting of text [string] with display
       attributes [attr]. [string] is assumed to be a valid UTF-8 sequence.

       @raise Invalid_argument if [string] contains control characters. *)

  val uchars : attr -> int array -> image
  (** [uchars attr array] is an image consisting of unicode characters in
      [array].

      @raise Invalid_argument if [array] contains control characters. *)

  val char : attr -> char -> int -> int -> image
  (** [char attr c w h] is a [w * h] grid with character [c] in every cell.

      @raise Invalid_argument if [c] is a control character. *)

  val uchar : attr -> [ `Uchar of uchar ] -> int -> int -> image
  (** [uchar attr (`Uchar u) w h] is a [w * h] grid with the unicode character
      [u] in every cell.

      @raise Invalid_argument if [u] is a control character. *)

  val void  : int -> int -> image
  (** [void w h] is a [w * h] rectangle of transparent cells.

      [void] is magical: it has geometry, but no displayable content. This is
      different, for example, from the space character [0x20], which renders as
      a cell filled with background color. This means that [void] interacts
      specially with {{!(<^>)}overlays}.

      It makes no sense to have negative [w] or [h] so [void] degenerates to
      [empty] in either case. Furthermore, [void 0 0 = empty]. It {e does} make
      sense to have a [void] which is [0] in one, and positive in the other
      dimension. *)

  (** {1 Image composition}

      Three basic composition modes allow construction of more complex images
      from simples ones.

      Images form a monoid under all three of {{!(<|>)}[<|>]}, {{!(<->)}[<->]}
      and {{!(<^>)}[<^>]}, with {{!empty}[empty]} as the identity. *)

  val (<|>) : image -> image -> image
  (**  [i1 <|> i2] is an image with [i1] to the left of [i2].

      It has [width = width i1 + width i2] and [height = max (height i1)
      (height i2)]. Images are top-aligned. The missing region is implicitly
      filled with {{!void}[void]}.

{v
i1 = [xx]  i1 <|> i2 = [xxy]
i2 = [y]               [..y]
     [y]
v}

      (Where '[.]' denotes {{!void}[void]}.) *)

  val (<->) : image -> image -> image
  (** [i1 <-> i2] is an image with [i1] above [i2].

      It has [width = max (width i1) (width i2)] and [height = height i1 +
      height i2]. Images are left-aligned. The missing region is implicitly
      filled with {{!void}[void]}.

{v
i1 = [xx]  i1 <-> i2 = [xx]
i2 = [y]               [y.]
     [y]               [y.]
v} *)

  val (<^>) : image -> image -> image
  (** [i1 <^> i2] is an image with [i1] overlaid over [i2].

      It has [width = max (width i1) (width i2)] and [height = max (height i1)
      (height i2)]. Images are top-left-aligned. In the region of their overlap,
      only the {{!void}[void]} cells of [i1] show fragments of [i2].

{v
i1 = [x.x]   i1 <^> i2 = [xyxy]
i2 = [yyyy]
v} *)

  (** {1 Cropping} *)

  val hcrop : int -> int -> image -> image
  (** [hcrop left right i] is [i] with [left] leftmost, and [right] rightmost
      columns missing. If [left + right >= width i] the result is [empty].

      If either [left] or [right] is negative, instead of being cropped, the
      image is padded on that side.

      For example:
      {ul
      {- [hcrop 1 1 [abc] = [b]]}
      {- [hcrop 0 1 [abc] = [ab]]}
      {- [hcrop (-1) 1 [abc] = void 1 1 <|> hcrop 0 1 [abc] = [.ab]]}
      {- [hcrop 2 2 [abc] = empty]}} *)

  val vcrop : int -> int -> image -> image
  (** [vcrop top bottom i] is analogous to {{!hcrop}[hcrop]}, but operating
      vertically. *)

  val crop  : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> image -> image
  (** [crop left right top bottom i = vcrop left right (hcrop top bottom i)]

      Missing parameters default to [0]. *)

  (** {1 Derived combinators} *)

  val hpad : int -> int -> image -> image
  (** {{!hcrop}[hcrop]} with margins negated. *)

  val vpad : int -> int -> image -> image
  (** {{!vcrop}[vcrop]} with margins negated. *)

  val pad : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> image -> image
  (** {{!crop}[crop]} with margins negated. *)

  val hcat : image list -> image
  (** [hcat xs] horizontally concatenates [xs]. See {{!(<|>)}beside}. *)

  val vcat : image list -> image
  (** [vcat xs] vertically concatenates [xs]. See {{!(<->)}above}. *)

  val zcat : image list -> image
  (** [zcat xs] overlays [xs]. See {{!(<^>)}over}. *)

  val tile : int -> int -> image -> image
  (** [tile m n i] is a grid of [m] horizontal and [n] vertical repetitions
      of [i]. *)

  val hframe : ?align:[ `Left | `Middle | `Right ] -> int -> image -> image
  (** [hframe ~align w i] is an image of width strictly [w] obtained by either
      horizontally padding or cropping [i] and positioning it according to
      [align]. *)

  val vframe : ?align:[ `Top | `Middle | `Bottom ] -> int -> image -> image
  (** [vframe ~align h i] is an image of height strictly [h] obtained by either
      vertically padding or cropping [i] and positioning it according to
      [align]. *)
end

(** Terminal capabilities.

    This module describes how to output things so that a terminal understands
    them. *)
module Cap : sig

  type t
  (** A set of capabilities that distinguish terminals from one another.

      A bundle of magic strings, really. *)

  val ansi : t
  (** The usual ANSI terminal, with colors, text styles and cursor
      positioning. *)

  val dumb : t
  (** Pure text output. Text attributes are stripped and positioning is done
      with the character [0x20]. *)
end

(** Convert images to [string].

    For use when you conclude that the output facilities are inadequate and that
    you want to take your business elsewhere. *)
module Render : sig

  val to_string : Cap.t -> (int * int) -> image -> string
  (** [to_string cap (w, h) i] is the string describing the [w * h] top-left
      rectangle of [i], as interpreted by {{!Cap}[cap]}. [i] is implicitly
      padded with {{!I.void}empty space} along the bottom and right edges. *)

  val to_buffer : Cap.t -> (int * int) -> image -> Buffer.t -> unit
  (** [to_buffer cap (w, h) i buf] renders [i] to a buffer. See
      {{!to_string}to_string}. *)
end

(** Directly deal with escape sequences in the input.

    Demultiplex escape sequences from characters in byte streams. Useful for
    building input abstractions other than {!Terminal}. *)
module Unescape : sig

  type esc = [ `C0 of char | `C1 of string | `Cseq of string ]

  type key = [
      `Up | `Down | `Right | `Left
    | `Pg_up | `Pg_dn
    | `Ins | `Del
    | `Home | `End
    | `Fn of int
(*     | `C0 of char *)
    | `Bs
    | `Enter
    | `Tab
  ]

  val key_of_control_code : esc -> key option
  (** [key_of_control_code seq] is the interpretation of the {{!esc}escape
      sequence} [seq] as a {{!key}special key}, if any. *)

  type t
  (** Input decoding filter. *)

  val create : unit -> t
  (** [create ()] is a new, empty {{!t}filter}. The filter should be
      {{!input}fed} strings, which it first decodes from UTF-8, and then
      separates control sequences from other characters.

      Malformed UTF-8 input bytes are discarded. *)

  val input : t -> string -> int -> int -> unit
  (** [input t string i j] feeds [string] into the filter's input buffer.
      [j = 0] signals the end of input.

      [input] should be called on a particular filter only after it generated
      [`Await], and it should not be called again until the next [`Await]. *)

  val next : t -> [ `End | `Await | `Esc of esc | `Uchar of uchar | `Malformed of string ]
  (** [next t] is the next event in the filter's input stream.

      {ul
      {- [`End] means end of input.}
      {- [`Await] means that the filter expects {{!input}more input}.}
      {- [`Malformed s] is an ill-formed escape sequence.}
      {- [`Esc e] is a well-formed escape sequence.}
      {- [`Uchar u] is a unicode character.}} *)

  val next_k : t -> [ `End | `Await | `Key of key | `Uchar of uchar ]
  (** [next_k t] behaves like {{!next}[next]}, except that it interprets the
      escape sequences as special keys. Unrecognized and [`Malformed]
      sequences are silently discarded. *)
end

(**/**)
(** Core rasterizer.

    {b Note} This is a private interface. *)
module Operation : sig
  type t
  val of_image : (int * int) -> image -> t list list
end

(** IO-less model of full-screen terminal output.

    {b Note} This is a private interface. *)
module Tmachine : sig

  type t

  val create  : Cap.t -> t
  val finish  : t -> bool
  val output  : t -> [ `Output of string | `Await ]

  val refresh  : t -> unit
  val resize   : t -> (int * int) -> unit
  val cursor   : t -> (int * int) option -> unit
  val image    : t -> image -> unit

  val set_size : t -> (int * int) -> unit

  val size : t -> (int * int)
end

(** Random handy bits for writing outputs.

    {b Note} This is a private interface. *)
module IO_helpers : sig

  val winsize : Unix.file_descr -> (int * int) option
  val cap_for_fd : Unix.file_descr -> Cap.t
  val setup_tcattr : Unix.file_descr -> [`Revert of (unit -> unit)]
  val set_winch_handler : (unit -> unit) -> [ `Revert of (unit -> unit) ]
  val output_image_gen :
    to_fd:('fd -> Unix.file_descr) ->
    write:('fd -> Buffer.t -> 'r) ->
    ?cap:Cap.t -> 'fd -> image -> 'r
end
(**/**)

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
         using the appropriate escape sequence.}
      {- [SIGWINCH] signal, normally ignored, is handled. This means that
         IO in your program will now be interrupted by [EINTR] on every window
         resize.}}

      [dispose] arranges for automatic {{!release}cleanup} of the terminal
      before the process terminates. The downside is that a reference to this
      terminal is retained until the program exits. Default to [true].

      [autosize] activates automatic {{!redraw}redrawing} of output whenever
      the window is resized. Defaults to [true].

      [input] is the input file descriptor. Defaults to [stdin].

      [output] is the output file descriptor. Defaults to [stdout].

      {{!Cap}Capabilities} are determined the same as with {!output_image}.

      {b Note} It is probably a poor idea to attach several {{!t}terminals} to
      the same [input] or [output]. *)

  val release : t -> unit
  (** Dispose of this terminal. Original behavior of input is reinstated, the
      output is cleared, cursor is restored and alternate mode is terminated.
      (See {{!create}create}.) *)

  (** {1 Commands} *)

  val image : t -> image -> unit
  (** Sets a new {{!Notty.image}[image]} and redraws the terminal. *)

  val redraw : t -> unit
  (** Redraws the terminal. Useful if the output might have become garbled. *)

  val cursor  : t -> (int * int) option -> unit
  (** Sets and redraws the cursor. [None] is hidden cursor. [Some (col, row)]
      is cursor at column [col] and row [row]. The origin is at [(1, 1)] in the
      upper-left corner. *)

  (** {1 Input} *)

  val input : t -> [ `End | `Uchar of uchar | `Key of Unescape.key ]
  (** Wait for new input to arrive.

      {ul
      {- [`End] means the input stream has ended.}
      {- [`Uchar u] is the next unicode character in the input.}
      {- [`Key k] is the next special key.}}

      See {!Unescape.next_k}.

      If [t] has {{!create}[autosize]}, [input] will silently restart and mask
      any [SIGWINCH] delivered while the call is in progress. *)

  (** {1 Properties} *)

  val size : t -> (int * int)
  (** [size t] is the current size of the terminal's output tty. *)

  (** {1 Window size change notifications} *)

  (** Manual [SIGWINCH] handling.

      Unix delivers notifications about tty size changes through the [SIGWINCH]
      signal. A handler for this signal is installed as soon as a new terminal
      is {{!create}created}. Replacing this global [SIGWINCH] handler through
      the [Sys] module will cause this module to malfunction, as size change
      notifications will no longer be delivered.

      You might still wish to intercept this signal, however. For example, if
      you are using a more complex event loop, you might want to disable
      {{!create}[autosize]} and manually schedule {{!redraw}terminal updates}.

      This module allows one to listen to [SIGWINCH] without conflicting with
      the rest of the machinery. *)
  module Winch : sig

    type remove
    (** Removal token. *)

    val add : Unix.file_descr -> ((int * int) -> unit) -> remove
    (** [add fd f] registers a [SIGWINCH] handler. Every time the signal is
        delivered, [f] is called with the current size of the tty backing [fd].
        If [fd] is not a tty, [f] is never called.

        Handlers are called in the order of their registration. *)

    val remove : remove -> unit
    (** [remove r] removes the handler associated with the removal token [r].
        Does nothing if the handler was already removed. *)
  end
end

val output_image : ?cap:Cap.t -> out_channel -> image -> unit
(** [output_image ~cap channel i] writes the image [i] to [channel].

    Height of the output is the height of [i], while the width is width of the
    tty out_channel is backed by, or width of [i] if this is not the case.

    Note that no leading or trailing characters are produced.
    This means that:
    {ul
    {- an image 1-cell high can be a part of a line of text, preceded and/or
       followed by any other output;}
    {- if the cursor is first positioned on the first column, image of any
       height can be printed; and}
    {- simply outputting an image higher than 1 when the cursor has advanced
       past colum 1 will result in broken graphics. }}

    Auto-detection for [cap] merely checks that both the environment variable
    [$TERM] is set, and the file descriptor backing [channel] [Unix.isatty]. If
    both are true, {{!Cap.ansi}ANSI} escapes are used. Otherwise,
    {{!Cap.dumb}no} escapes are used. *)

val print_image : image -> unit
(** {{!output_image}Output image} to [stdout]. *)


(** {1 Examples}

{b Note} There are further examples in the [/demos] directory in the source
tree.

{{!print_image}[print_image]} does not add a newline, so assume a helper:

{[let print_endl i = print_image i; print_char '\n' ]}
*)

(** {2 Hello}

Output "rad" with default foreground and background:

{[let () = print_endl (I.string A.empty "rad") ]} *)

(** {2 Hello, with colors}

Output "rad" in rad letters:

{[let () = print_endl (I.string A.(fg red) "rad") ]}
*)

(** {2 Padding and spacing}

Output "rad" and "stuff" in different colors and with a space between:

{[let () =
  let i = I.(
    string A.(fg red) "rad " <|> string A.(white @/ bg red) "stuff"
  ) in print_endl i]}

Output "rad stuff" with the second word hanging on a line below:

{[let () =
  let attr = A.(white @/ bg red) in
  let i = I.(
    string attr "rad" <|> pad ~top:1 ~left:1 (string attr "stuff")
  ) in print_endl i]}
*)

(** {2 More geometry}

Sierpinski triangle:

{[
let rec sierp n = I.(
  if n > 1 then
    let ss = sierp (pred n) in ss <-> (ss <|> ss)
  else hpad 1 0 (string A.(fg magenta) "◾")
)]}

{b Note} Square is really the byte sequence ["\226\151\190"] in the source text,
i.e. the source is UTF-8 encoded.

Print a triangle:

{[let () = print_endl (sierp 5)]}

Print a triangle overlaid over its copy shifted one cell to the right:

{[let () =
  let s = sierp 5 in
  print_endl I.(s <^> hpad 1 0 s)
]}

Blinkenlights:

{[
let rad n color =
  let attr = A.(fg color) in
  I.((void n 0 <|> string A.(blink @+ attr) "rad")
      <-> (void (n + 6) 0 <|> string attr "stuff"))
let image =
  A.[red; green; yellow; blue; magenta; cyan]
  |> List.mapi I.(fun i c -> pad ~top:i ~left:(2*i) (rad i c))
  |> I.zcat
let () = print_endl image
]}

{b Note} Usage of {{!A.blink}[blink]} might be regulated by law in some
jurisdictions.

*)

(** {2 Simple interaction}

{[
let () =
  let img (double, n) =
    let s = sierp n in
    if double then I.(s <^> hpad 1 0 s) else s in
  let rec update t state =
    Terminal.image t (img state); loop t state
  and loop t (double, n as state) =
    match Terminal.input t with
    | `Key `Left  -> update t (double, max 1 (n - 1))
    | `Key `Right -> update t (double, min 8 (n + 1))
    | `Key `Enter -> ()
    | `Uchar 0x20 -> update t (not double, n)
    | _           -> loop t state
  in
  let t = Terminal.create () in
  update t (false, 1)
]} *)
