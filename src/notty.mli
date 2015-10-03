
type uchar = int
type attr
type image

module A : sig

  type style

  val bold      : style
  val italic    : style
  val underline : style
  val blink     : style
  val reverse   : style

  type color

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

  val to_index : color -> int
  val of_index : int -> color

  val rgb  : r:int -> g:int -> b:int -> color
  val gray : level:int -> color

  val empty : attr
  val (&) : attr -> attr -> attr

  val fg : color -> attr
  val bg : color -> attr
  val st : style -> attr

  val (@/)  : color -> attr -> attr
  val (@//) : color -> attr -> attr
  val (@+)  : style -> attr -> attr

  val equal : attr -> attr -> bool
end

module I : sig

  val height : image -> int
  val width  : image -> int

  val empty  : image
  val space  : int -> int -> image
  val string : attr -> string -> image
  val uchars : attr -> int array -> image
  val char   : attr -> char -> int -> int -> image
  val uchar  : attr -> [ `Uchar of uchar ] -> int -> int -> image

  val (<|>) : image -> image -> image
  val (<->) : image -> image -> image
  val (<^>) : image -> image -> image

  val hcrop : int -> int -> image -> image
  val vcrop : int -> int -> image -> image
  val crop  : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> image -> image

  val hpad : int -> int -> image -> image
  val vpad : int -> int -> image -> image
  val pad  : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> image -> image

  val hcat : image list -> image
  val vcat : image list -> image
  val zcat : image list -> image

  val tile : int -> int -> image -> image

  val hframe : ?align:[ `Left | `Middle | `Right ] -> int -> image -> image
  val vframe : ?align:[ `Top | `Middle | `Bottom ] -> int -> image -> image

end

module Operation : sig
  type t
  val of_image : (int * int) -> image -> t list list
end

module Cap : sig
  type t
  val ansi : t
  val dumb : t
end

module Render : sig
  val to_buffer : Cap.t -> (int * int) -> image -> Buffer.t -> unit
  val to_string : Cap.t -> (int * int) -> image -> string
end

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

  type res = [
    | `Uchar     of uchar
    | `Esc       of esc
    | `Malformed of string
    | `Await
    | `End
  ]

  val key_of_control_code : esc -> key option

  type t

  val create : unit -> t

  val next : t -> [ `End | `Await | `Malformed of string | `Esc of esc | `Uchar of uchar ]
  val next_k : t -> [ `End | `Await | `Key of key | `Uchar of uchar ]

  val input : t -> string -> int -> int -> unit
end

module Tmachine : sig

  type t

  val create  : Cap.t -> t
  val finish  : t -> bool
  val output  : t -> [ `Output of string | `Await ]

  val refresh : t -> unit
  val resize  : t -> (int * int) -> unit
  val cursor  : t -> (int * int) option -> unit
  val image   : t -> (int * int) option -> image -> unit

  val size    : t -> (int * int)
end

module IO_helpers : sig

  val winsize : Unix.file_descr -> (int * int) option
  val cap_for_fd : Unix.file_descr -> Cap.t
  val setup_tcattr : Unix.file_descr -> [`Revert of (unit -> unit)]
  val set_winch_handler : (unit -> unit) -> [ `Revert of (unit -> unit) ]
end

module Terminal : sig

  type t
  type event = [ `End | `Uchar of uchar | `Key of Unescape.key ]

  val create : ?dispose:bool ->
               ?winch:bool ->
               ?input:Unix.file_descr ->
               ?output:Unix.file_descr ->
               unit -> t

  val release : t -> unit

  val resize  : t -> (int * int) -> unit
  val input_w : t -> [ `Winch of (int * int) | event ]
  val input   : t -> event
  val refresh : t -> unit
  val update  : t -> ?cursor:(int * int) -> image -> unit

  val size : t -> (int * int)

  module Input : sig
    type t
    val create  : Unix.file_descr -> t
    val release : t -> unit
    val input   : t -> event
  end
end

val output_image : ?cap:Cap.t -> out_channel -> image -> unit
val print_image  : image -> unit
