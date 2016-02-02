
(** Declaring terminals.

    Notty is a terminal library that revolves around construction and
    composition of displayable {{!I}images}.

    This module provides the core {{!I}image} abstraction, and standalone
    {{!Render}rendering} and escape sequence {{!Unescape}parsing}. It does not
    depend on any platform code, and does not interact with the environment.
    Input and output are provided by {!Notty_unix} and {!Notty_lwt}.

    {b Note} [Notty] assumes that the terminal is using UTF-8 for input and
    output. Things might break arbitrarily if this is not the case.

    {b Note} [Notty] does not use terminfo. If your terminal is particularly
    idiosyncratic, things might fail to work. Get in touch with the author to
    expand support. *)


(** {1 Interface} *)

type uchar = int
(** A lone unicode
   {{: http://unicode.org/glossary/#unicode_scalar_value}scalar value}. *)

type attr
(** Visual characteristics of displayed text. *)

type image
(** Things that can be output to a terminal. *)

(** [A] is for attribute.

    Construction and composition of visual characteristics of text. *)
module A : sig

  (** {1 Colors}

      [Notty] uses 256 {e xterm-style} colors.

      The first 16 are supported on most terminals. Their names are
      standardized, but the actual colors are not, and are user-definable on
      many terminal emulators.

      The next 216 form the 6*6*6 {e color cube}.

      The final 24 are the {e grayscale ramp}.

      {b Note} Presently, no attempt is made to remap colors depending on the
      terminal. This means that the colors not recognized by a particular
      terminal will simply be ignored in the output. *)

  type color
  (** One of the 256 terminal colors. *)

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

  (** {1 Attribute construction and composition} *)

  (** {{!attr}Attribute} describes visual appearance of fragments of text.

      Attributes combine a foreground and a background {{!color}color} with a
      set of {{!style}styles}. Either color can be {e missing} from an
      attribute, in which case the terminal's default foreground (resp.
      background) is used.

      They are used to construct primitive {{!I}images}. *)

  val empty : attr
  (** [empty] is the attribute with no foreground or background color and with
      an empty style set. *)

  val (&) : attr -> attr -> attr
  (** [a1 & a2] is the attribute that has [a2]'s foreground (resp. background)
      if not {e missing}, or [a1]'s otherwise, and the union of both style sets.

      {{!attr}[attr]} forms a monoid under {{!empty}[empty]} and {{!(&)}[&]}. *)

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

      {1:ctrls Control characters}

      These are taken to be characters in the ranges [0x00-0x1f] ({b C0}) and
      [0x80-0x9f] ({b C1}), and [0x7f] (BACKSPACE). This is the
      {{: http://unicode.org/glossary/#general_category}Unicode general
      category} {b Cc}.

      As control characters directly influence the cursor positioning, they
      cannot be used to create images.

      {1:cwidth Unicode vs. Text geometry}

      [Notty] uses [Uucp.Break.tty_width_hint] to guess the width of text
      fragments when computing geometry, and it suffers from the same
      shortcomings:

      {ul
      {- Geometry in general works for alphabets and east asian scripts, mostly
         works for abjad scripts, and is a matter of luck for abugidas.}
      {- East asian scripts work better when in
         {{:http://unicode.org/glossary/#normalization_form_c}NFC}.}
      {- Emoji tend to be consistent with the actual rendering, and the actual
         rendering tends to be wrong.}}

      When in doubt, see
      {{: http://erratique.ch/software/uucp/doc/Uucp.Break.html#VALtty_width_hint}
      [Uucp.Break.tty_width_hint]}.

      Unicode also has a special interaction with {{!hcrop}horizontal cropping}:
      {ul
      {- Strings within images are cropped at {{:
         http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries}grapheme
         cluster} boundaries. This means that scalar value sequences that are
         rendered combined, or overlaid, stay unbroken.}
      {- When a crop splits a wide character in two, the remaining half is
         replaced by [0x20] (SPACE). Hence, character-cell-accurate cropping is
         possible even in the presence of characters that horizontally occupy
         more than one cell.}}

      {1 Image properties} *)

  val height : image -> int
  val width  : image -> int

  (** {1 Primitive constructors} *)

  val empty : image
  (** [empty] is a zero-sized image. *)

  val string : attr -> string -> image
  (** [string attr string] is an image consisting of text [string] with display
       attributes [attr].

       @raise Invalid_argument if [string] is not a valid UTF-8 sequence, or
       contains {{!ctrls}control characters}. *)

  val uchars : attr -> int array -> image
  (** [uchars attr array] is an image consisting of unicode characters in
      [array].

      @raise Invalid_argument if [array] contains {{!ctrls}control
      characters}.  *)

  val char : attr -> char -> int -> int -> image
  (** [char attr c w h] is a [w * h] grid with character [c] in every cell.

      @raise Invalid_argument if [c] is a {{!ctrls}control character}. *)

  val uchar : attr -> uchar -> int -> int -> image
  (** [uchar attr u w h] is a [w * h] grid with the unicode character
      [u] in every cell.

      @raise Invalid_argument if [u] is a {{!ctrls}control character}. *)

  val void  : int -> int -> image
  (** [void w h] is a [w * h] rectangle of transparent cells.

      [void] is magical: it has geometry, but no displayable content. This is
      different, for example, from the space character [0x20], which renders as
      a cell filled with the background color. This means that [void] interacts
      specially with {{!(<^>)}overlays}.

      A negative size is treated as [0]. [void 0 0] is [empty]. Void with only
      one dimension [0] acts as a spacing element in the other dimension. *)

  (** {1 Image composition}

      Three basic composition modes allow construction of more complex images
      from simpler ones.

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
      {- [hcrop 0 1 [abc] = [ab]]}
      {- [hcrop 1 1 [abc] = [b]]}
      {- [hcrop (-1) 1 [abc] = void 1 1 <|> hcrop 0 1 [abc] = [.ab]]}
      {- [hcrop 2 2 [abc] = empty]}
      {- [hcrop 2 2 [a] = void]}} *)

  val vcrop : int -> int -> image -> image
  (** [vcrop top bottom i] is analogous to {{!hcrop}[hcrop]}, but operating
      vertically. *)

  val crop : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> image -> image
  (** {v crop left right top bottom i = vcrop left right (hcrop top bottom i) v}

      Missing parameters default to [0]. *)

  (** {1 Derived combinators} *)

  val stringp : attr -> ('a, unit, bytes, image) format4 -> 'a
  (** [stringp attr fmt p1 ...] is the image containing the string
      [sprintf fmt p1 ...]. *)

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

  val hlimit : ?align:[ `Left | `Middle | `Right ] -> int -> image -> image
  (** [hlimit ~align w i] is an image of width strictly [w] obtained by either
      horizontally padding or cropping [i] and positioning it according to
      [align]. *)

  val vlimit : ?align:[ `Top | `Middle | `Bottom ] -> int -> image -> image
  (** [vlimit ~align h i] is an image of height strictly [h] obtained by either
      vertically padding or cropping [i] and positioning it according to
      [align]. *)
end

(** {1 Low-level interface}

    You can ignore it, unless you are porting [Notty] to a new platform not
    supported by the existing IO backends. *)

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

    Escape sequence demultiplexer and decoder. Useful for building custom
    terminal input abstractions. *)
module Unescape : sig

  type special = [
    `Escape
  | `Enter
  | `Tab
  | `Backspace
  | `Up | `Down | `Left | `Right
  | `Pg_up | `Pg_dn | `Home | `End
  | `Insert | `Delete
  | `Fn of int
  ]
  (** A selection of extra keys on the keyboard. *)

  type button = [ `LMB | `MMB | `RMB | `Scroll_up | `Scroll_dn ]
  (** Mouse buttons. *)

  type mods = [ `Meta | `Ctrl ] list
  (** Modifier state. *)

  type event = [
  | `Key   of [ special | `Uchar of uchar ] * mods
  | `Mouse of [ `Press of button | `Drag | `Release ] * (int * int) * mods
  ]
  (** Things that terminals say to applications.

      {ul
      {- [`Key (k, mods)] is keyboard input.

         [k] is either a {{!special}special key}, or [`Uchar u] where [u] is
         {!uchar}. This value is guaranteed not to be a
         {{!I.ctrls}control character}, and is safe to directly use in
         constructing {!image}s.

         [mods] are the extra {{!mods}modifier keys}.

         }
      {- [`Mouse (event, (x, y), mods)] is mouse input.

         [event] is the actual mouse event: {!button} press, release, or motion
         of the mouse with buttons depressed.

         [(x, y)] are colum and row position of the mouse. The origin is
         [(1,1)], the upper-left corner.

         {b Note} Every [`Press (`LMB|`MMB|`RMB)] generates a corresponding
         [`Release], but there is no portable way to detect which button was
         released. [`Scroll_up] and [`Scroll_dn] presses are not followed by
         releases.

         }}

      Terminal input protocols are historical cruft, and heavily overload the
      ASCII range. For instance:
      {ul
      {- It is impossible to distinguish lower- and upper-case ASCII
         characters if {b Ctrl} is pressed;}
      {- several combinations of key-presses are aliased as special keys; and}
      {- in a UTF-8 encoded stream, there is no representation for non-ASCII
          characters with modifier keys.}}

      This means that many values that inhabit the [event] type are impossible
      in practice, while some denote multiple different user actions.

      {b Note} Terminals vary widely in their capability, or willingness, to
      signal modifier keys. Perform own experiments before relying on elaborate
      combinations. *)

  val decode : uchar list -> event list
  (** [decode ucs] gives the events represented by [ucs].

      [ucs] are assumed to have been generated in a burst, and the end of the
      list is taken to mean a pause.
      Therefore, [decode us1 @ decode us2 <> decode (us1 @ us2)] if [us1] ends
      with a partial escape sequence, including a lone [\x1b].

      Unsupported escape sequences are silently discarded. *)

  type t
  (** Input decoding filter.

      The filter should be {{!input}fed} strings, which it first decodes from
      UTF-8, and then extracts the input events.

      Malformed UTF-8 input bytes and unrecognized escape sequences are silently
      discarded. *)

  val create : unit -> t
  (** [create ()] is a new, empty filter. *)

  val input : t -> string -> int -> int -> unit
  (** [input t string i j] feeds [j] bytes of [string] into [t], starting from
      position [i].

      [j = 0] signals the end of input. *)

  val next : t -> [ event | `Await | `End ]
  (** [next t] is the next {!event} in the filter's input stream:

      {ul
      {- [`Await] means that the filter needs {{!input}more input}.}
      {- [`End] means that the input had ended.}
      {- [#event] is an {{!event}input event}.}} *)

  val pending : t -> bool
  (** [pending t] is [true] if a call to [next], without any intervening input,
      would {e not} return [`Await]. *)

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
  val release : t -> bool
  val output  : t -> [ `Output of string | `Await ]

  val refresh  : t -> unit
  val resize   : t -> (int * int) -> unit
  val cursor   : t -> (int * int) option -> unit
  val image    : t -> image -> unit

  val set_size : t -> (int * int) -> unit

  val size : t -> (int * int)
  val dead : t -> bool
end
(**/**)

(** {1 Examples}

{b Note} There are further examples in the [/demos] directory in the source
tree.

We assume the module has been opened:

{[open Notty]}

As the core module has no IO, we borrow a helper from {!Notty_unix}:

{[let print_image_nl = Notty_unix.print_image_nl]}

{2 Hello}

Output ["rad"] with default foreground and background:

{[print_image_nl (I.string A.empty "rad") ]}

{2 Hello, with colors}

Output ["rad"] in rad letters:

{[print_image_nl (I.string A.(fg lightred) "rad") ]}

{2 Padding and spacing}

Output ["rad"] and ["stuff"] in different colors and with a space between:

{[
let i = I.(
  string A.(fg red) "rad " <|> string A.(white @/ bg red) "stuff"
) in print_image_nl i
]}

Output ["rad stuff"] with the second word hanging on a line below:

{[
let attr = A.(white @/ bg red) in
let i = I.(
  string attr "rad" <|> pad ~top:1 ~left:1 (string attr "stuff")
) in print_image_nl i
]}

{2 More geometry}

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

{[print_image_nl (sierp 7)]}

Print a triangle overlaid over its shifted copy:

{[let s = sierp 7 in print_image_nl I.(s <^> hpad 1 0 s)]}

Blinkenlights:

{[
let rad n color =
  let attr = A.(fg color) in
  I.((void n 0 <|> string A.(blink @+ attr) "rad")
      <-> (void (n + 6) 0 <|> string attr "stuff")) in
let image =
  A.[red; green; yellow; blue; magenta; cyan]
  |> List.mapi I.(fun i c -> pad ~top:i ~left:(2*i) (rad i c))
  |> I.zcat in
print_image_nl image
]}

{b Note} Usage of {{!A.blink}[blink]} might be regulated by law in some
jurisdictions.

{2 Taking terminal size into account}

Space a line end-to-end horizontally:

{[
Notty_unix.print_image_f I.(fun (w, _) ->
  hcat [ string A.(fg red) "very"
       ; void (w - 8) 1
       ; string A.(fg green) "wide" ])
]}

Print a triangle that fits into the terminal:

{[
Notty_unix.print_image_f @@ fun (w, _) ->
  let steps = int_of_float ((log (float w)) /. log 2.) in
  sierp steps |> I.vpad 0 1
]}


{2 Simple interaction}

{[
open Notty_unix

let img (double, n) =
  let s = sierp n in
  if double then I.(s <^> hpad 1 0 s) else s in
let rec update t state =
  Terminal.image t (img state); wait t state
and wait t (double, n as state) =
  match Terminal.input t with
  | `Key (`Enter,_)      -> ()
  | `Key (`Left,_)       -> update t (double, max 1 (n - 1))
  | `Key (`Right,_)      -> update t (double, min 8 (n + 1))
  | `Key (`Uchar 0x20,_) -> update t (not double, n)
  | _                    -> wait t state
in
let t = Terminal.create () in
update t (false, 1);
Terminal.release t
]}

*)
