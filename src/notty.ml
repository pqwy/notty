
let (&.) f g x = f (g x)

let btw (x : int) a b = a <= x && x <= b
let bit n b = b land (1 lsl n) > 0

let invalid_arg_s fmt = Printf.ksprintf invalid_arg fmt

let maccum ~empty ~append xs =
  let rec step = function
    | []  -> empty
    | [x] -> x
    | xs  -> step (accum xs)
  and accum = function
    | []|[_] as xs -> xs
    | a::b::xs     -> append a b :: accum xs
  in step xs

module Queue = struct

  include Queue

  let addv q xs = List.iter (fun x -> add x q) xs
  let of_list xs = let q = create () in addv q xs; q
  let singleton x = of_list [x]
end

module List = struct

  include List

  let rec replicate n a = if n < 1 then [] else a :: replicate (n - 1) a
  let rec (--) a b = if a > b then [] else a :: succ a -- b
end

module Buffer = struct

  include Buffer

  let on size f = let buf = create size in f buf; contents buf

  let add_decimal b = function
    | x when btw x 0 999 ->
        let d1 = x / 100 and d2 = (x mod 100) / 10 and d3 = x mod 10 in
        if d1 > 0 then 0x30 + d1 |> Char.unsafe_chr |> add_char b;
        if (d1 + d2) > 0 then 0x30 + d2 |> Char.unsafe_chr |> add_char b;
        0x30 + d3 |> Char.unsafe_chr |> add_char b
    | x -> string_of_int x |> add_string b

  let add_chars b c n = for _ = 1 to n do add_char b c done
end

module Ucharp = struct
  let is_ctrl  u = btw u 0x00 0x1f || btw u 0x7f 0x9f
  let is_ascii u = u = u land 0x7f
end

module Char = struct
  include Char
  let is_ctrl  c = Ucharp.is_ctrl  (code c)
  let is_ascii c = Ucharp.is_ascii (code c)
end

module String = struct

  include String

  let sub0cp s i len = if i > 0 || len < length s then sub s i len else s

  let for_all f s =
    let rec go f s i = i < 0 || f s.[i] && go f s (i - 1) in
    go f s (length s - 1)

  let find f s =
    let rec go f s i = function
      | 0 -> None | _ when f s.[i] -> Some i | n -> go f s (i + 1) (n - 1)
    in go f s 0 (length s)

  let of_uchars_rev = function
    | []  -> ""
    | [u] -> String.make 1 (Char.chr u)
    | ucs ->
        let n = List.length ucs in
        let rec go bs i = function
          | []    -> Bytes.unsafe_to_string bs
          | x::xs -> Bytes.unsafe_set bs i (Char.chr x); go bs (pred i) xs
        in go (Bytes.create n) (n - 1) ucs
end

module Int = struct

  type t = int

  let max (a : t) (b : t) = if a > b then a else b
  let min (a : t) (b : t) = if a < b then a else b
end

module Option = struct

  let map f = function Some x -> Some (f x) | _ -> None
  let get def = function Some x -> x | _ -> def
  let to_list = function Some x -> [x] | _ -> []
  let (>>|) a f = map f a
  let (>>=) a f = match a with Some x -> f x | _ -> None
end

module Utf8 = struct

  let on_encoder ?(hint=16) f =
    Buffer.on hint @@ fun buf ->
      let enc = Uutf.encoder `UTF_8 (`Buffer buf) in
      f enc; Uutf.encode enc `End |> ignore

  let of_uchars arr =
    let n = Array.length arr in
    on_encoder ~hint:n @@ fun enc ->
      for i = 0 to n - 1 do Uutf.encode enc (`Uchar arr.(i)) |> ignore done
end

module Text = struct

  open Result

  type t =
    | Ascii of string * int * int
    | Utf8  of string * int array * int * int

  let equal t1 t2 = match (t1, t2) with
    | (Utf8 (s1, _, i1, n1), Utf8 (s2, _, i2, n2))
    | (Ascii (s1, i1, n1), Ascii (s2, i2, n2)) -> i1 = i2 && n1 = n2 && s1 = s2
    | _ -> false

  let width = function Utf8 (_, _, _, w) -> w | Ascii (_, _, w)   -> w

  let empty = Ascii ("", 0, 0)

  let is_empty t = width t = 0

  let graphemes ?encoding str =
    let dec = Uutf.decoder ?encoding (`String str)
    and seg = Uuseg.create `Grapheme_cluster in
    let rec go is w i evt =
      match Uuseg.add seg evt with
      | `Await ->
        ( let i = Uutf.decoder_byte_count dec in
          match Uutf.decode dec with
          | `Await -> assert false
          | `Malformed _ as err -> Error err
          | `End | `Uchar _ as evt -> go is w i evt )
      | `Boundary ->
          let is = match w with 0 -> is | 1 -> i::is | _ -> i::(-1)::is
          in go is 0 0 `Await
      | `Uchar u when Ucharp.is_ctrl (Uchar.to_int u) -> Error (`Control u)
      | `Uchar u -> go is (w + Uucp.Break.tty_width_hint u) i `Await
      | `End -> Ok (is |> List.rev |> Array.of_list) (* XXX *)
    in
    go [0] 0 0 `Await

  let dead = ' '

  let to_buffer buf = function
    | Ascii (s, off, w)    -> Buffer.add_substring buf s off w
    | Utf8 (s, ix, off, w) ->
        let x1 = match ix.(off) with
          | -1 -> Buffer.add_char buf dead; ix.(off + 1) | x -> x
        and x2 = ix.(off + w) in
        Buffer.add_substring buf s x1 @@
          (if x2 = -1 then ix.(off + w - 1) else x2) - x1;
        if x2 = -1 then Buffer.add_char buf dead

  let sub t x w =
    let w1 = width t in
    if w = 0 || x >= w1 then empty else
      let w = Int.min w (w1 - x) in
      match t with
      | Ascii (s, off, _) -> Ascii (s, off + x, w)
      | Utf8 (s, ix, off, _) -> Utf8 (s, ix, off + x, w)

  open Char

  let err_ctrl_uchar = invalid_arg_s "Notty: control char: U+%04X (in: %S)"
  let err_malformed  = invalid_arg_s "Notty: malformed UTF-8: %s (in: %S)"

  let of_ascii str =
    match String.find is_ctrl str with
    | Some i -> err_ctrl_uchar (code str.[i]) str
    | None   -> Ascii (str, 0, String.length str)

  let of_unicode str =
    match graphemes ~encoding:`UTF_8 str with
    | Ok ix                  -> Utf8 (str, ix, 0, Array.length ix - 1)
    | Error (`Malformed err) -> err_malformed err str
    | Error (`Control u)     -> err_ctrl_uchar (Uchar.to_int u) str

  let of_string = function
    | "" -> empty
    | str when String.for_all is_ascii str -> of_ascii str
    | str -> of_unicode str

  let of_uchars = of_string &. Utf8.of_uchars

  let replicatec w c =
    if is_ctrl c then
      err_ctrl_uchar (code c) (Printf.sprintf "%d * %c" w c)
    else if w < 1 then empty else Ascii (String.make w c, 0, w)

  let replicateu w u =
    let u = Uchar.to_int u in
    if Ucharp.is_ascii u then replicatec w (chr u)
    else if w < 1 then empty
    else of_unicode @@ Utf8.on_encoder ~hint:w @@ fun enc ->
      let uchar = `Uchar (Uchar.unsafe_of_int u) in
      for _ = 1 to w do Uutf.encode enc uchar |> ignore done
end

module A = struct

  type color = int
  type style = int
  type t = { fg : color option; bg : color option; st : style }

  let equal t1 t2 =
    let f o1 o2 = match (o1, o2) with
      | (Some x, Some y) -> (x : int) = y | (None, None) -> true | _ -> false
    in f t1.fg t2.fg && f t1.bg t2.bg && t1.st = t2.st

  let black        = 0
  and red          = 1
  and green        = 2
  and yellow       = 3
  and blue         = 4
  and magenta      = 5
  and cyan         = 6
  and white        = 7
  and lightblack   = 8
  and lightred     = 9
  and lightgreen   = 10
  and lightyellow  = 11
  and lightblue    = 12
  and lightmagenta = 13
  and lightcyan    = 14
  and lightwhite   = 15

  let rgb ~r ~g ~b =
    if r < 0 || g < 0 || b < 0 || r > 5 || g > 5 || b > 5 then
      invalid_arg "Notty.A.rgb: a component outside of range [0, 5]"
    else r * 36 + g * 6 + b + 16

  let gray level =
    if level < 0 || level > 23 then
      invalid_arg "Notty.A.gray: level outside of range [0, 23]"
    else level + 232

  let bold      = 1
  and italic    = 2
  and underline = 4
  and blink     = 8
  and reverse   = 16

  let empty = { fg = None; bg = None; st = 0 }

  let (++) a1 a2 = {
    fg = (match a2.fg with None -> a1.fg | x -> x)
  ; bg = (match a2.bg with None -> a1.bg | x -> x)
  ; st = a1.st lor a2.st
  }

  let fg f = { empty with fg = Some f }
  let bg b = { empty with bg = Some b }
  let st s = { empty with st = s }

  let to_tag { fg; bg; st } =
    let f = ref 0 and chr = Char.unsafe_chr in
    let g k = function Some x -> f := !f + k; chr x | _ -> '\x00' in
    String.init 3 @@ function
      | 0 -> g 1 fg | 1 -> g 2 bg | _ -> (st lsl 2) lor !f |> chr

  let of_tag s =
    if String.length s <> 3 then None else
      let f = Char.code s.[2] in Some {
        fg = if bit 0 f then Some Char.(code s.[0]) else None
      ; bg = if bit 1 f then Some Char.(code s.[1]) else None
      ; st = f lsr 2 }
end

module I = struct

  type dim = int * int

  type t =
    | Empty
    | Segment  of A.t * Text.t
    | Hcompose of (t * t) * dim
    | Vcompose of (t * t) * dim
    | Zcompose of (t * t) * dim
    | Hcrop    of (t * int * int) * dim
    | Vcrop    of (t * int * int) * dim
    | Void     of dim

  let width = function
    | Empty -> 0
    | Segment (_, text) -> Text.width text
    | Hcompose (_, (w, _)) -> w
    | Vcompose (_, (w, _)) -> w
    | Zcompose (_, (w, _)) -> w
    | Hcrop    (_, (w, _)) -> w
    | Vcrop    (_, (w, _)) -> w
    | Void         (w, _)  -> w

  let height = function
    | Empty -> 0
    | Segment _ -> 1
    | Hcompose (_, (_, h)) -> h
    | Vcompose (_, (_, h)) -> h
    | Zcompose (_, (_, h)) -> h
    | Hcrop    (_, (_, h)) -> h
    | Vcrop    (_, (_, h)) -> h
    | Void         (_, h)  -> h

  let equal t1 t2 =
    let rec eq t1 t2 = match (t1, t2) with
      | (Empty, Empty) -> true
      | (Segment (a1, t1), Segment (a2, t2)) ->
          A.equal a1 a2 && Text.equal t1 t2
      | (Hcompose ((a, b), _), Hcompose ((c, d), _))
      | (Vcompose ((a, b), _), Vcompose ((c, d), _))
      | (Zcompose ((a, b), _), Zcompose ((c, d), _)) -> eq a c && eq b d
      | (Hcrop ((a, i1, n1), _), Hcrop ((b, i2, n2), _))
      | (Vcrop ((a, i1, n1), _), Vcrop ((b, i2, n2), _)) ->
          i1 = i2 && n1 = n2 && eq a b
      | (Void (a, b), Void (c, d)) -> a = c && b = d
      | _ -> false in
    width t1 = width t2 && height t1 = height t2 && eq t1 t2

  let empty = Empty

  let (<|>) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = width t1 + width t2
        and h = Int.max (height t1) (height t2) in
        Hcompose ((t1, t2), (w, h))

  let (<->) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = Int.max (width t1) (width t2)
        and h = height t1 + height t2 in
        Vcompose ((t1, t2), (w, h))

  let (</>) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = Int.max (width t1) (width t2)
        and h = Int.max (height t1) (height t2) in
        Zcompose ((t1, t2), (w, h))

  let void w h =
    if w < 1 && h < 1 then Empty else Void Int.(max 0 w, max 0 h)

  let lincropinv crop void (++) init fini img =
    match (init >= 0, fini >= 0) with
    | (true, true) -> crop init fini img
    | (true, _   ) -> crop init 0 img ++ void (-fini)
    | (_   , true) -> void (-init) ++ crop 0 fini img
    | _            -> void (-init) ++ img ++ void (-fini)

  let hcrop =
    let ctor left right img =
      let h = height img and w = width img - left - right in
      if w > 0 then Hcrop ((img, left, right), (w, h)) else void w h
    in lincropinv ctor (fun w -> void w 0) (<|>)

  let vcrop =
    let ctor top bottom img =
      let w = width img and h = height img - top - bottom in
      if h > 0 then Vcrop ((img, top, bottom), (w, h)) else void w h
    in lincropinv ctor (void 0) (<->)

  let crop ?(l=0) ?(r=0) ?(t=0) ?(b=0) img =
    let img = if l <> 0 || r <> 0 then hcrop l r img else img in
    if t <> 0 || b <> 0 then vcrop t b img else img

  let hpad left right img = hcrop (-left) (-right) img

  let vpad top bottom img = vcrop (-top) (-bottom) img

  let pad ?(l=0) ?(r=0) ?(t=0) ?(b=0) img =
    crop ~l:(-l) ~r:(-r) ~t:(-t) ~b:(-b) img

  let hcat = maccum ~empty ~append:(<|>)

  let vcat = maccum ~empty ~append:(<->)

  let zcat xs = List.fold_right (</>) xs empty

  let text attr tx =
    if Text.is_empty tx then void 0 1 else Segment (attr, tx)

  let string attr s = text attr (Text.of_string s)

  let uchars attr a = text attr (Text.of_uchars a)

  let chars ctor attr c w h =
    if w < 1 || h < 1 then void w h else
    text attr (ctor w c) |> List.replicate h |> vcat

  let char  = chars Text.replicatec
  let uchar = chars Text.replicateu

  let tile w h i = List.(replicate h (replicate w i |> hcat) |> vcat)

  let hsnap ?(align=`Middle) w img =
    let off = width img - w in match align with
      | `Left   -> hcrop 0 off img
      | `Right  -> hcrop off 0 img
      | `Middle -> let w1 = off / 2 in hcrop w1 (off - w1) img

  let vsnap ?(align=`Middle) h img =
    let off = height img - h in match align with
      | `Top    -> vcrop 0 off img
      | `Bottom -> vcrop off 0 img
      | `Middle -> let h1 = off / 2 in vcrop h1 (off - h1) img

  module Fmt = struct

    type ibuf = {
      mutable image : t;
      mutable line  : t list;
      mutable attrs : A.t list
    }

    let create_ibuf () = { image = empty; line = []; attrs = [] }

    let reset_ibuf ({image; _} as b) = b.image <- empty; image

    let flush_b ({image; line; _} as b) =
      b.image <- image <-> hcat (List.rev line); b.line <- []; b.attrs <- []

    open Format

    let formatter_of_image_buffer b =
      let attr () = match b.attrs with a::_ -> a | _ -> A.empty in
      let mark_open_tag tag =
        b.attrs <- Option.get (attr ()) (A.of_tag tag) :: b.attrs; ""
      and mark_close_tag _ =
        b.attrs <- (match b.attrs with _::t -> t | _ -> []); "" in
      let out_string s i l =
        b.line <- string (attr ()) String.(sub0cp s i l) :: b.line
      and out_spaces w = b.line <- void w 0 :: b.line
      and out_newline () =
        b.image <- b.image <-> hcat (List.rev b.line);
        b.line  <- [void 0 1]
      and out_flush () = flush_b b in
      let fmt = make_formatter out_string out_flush in
      pp_set_formatter_out_functions fmt {
        out_flush; out_string; out_spaces; out_newline };
      pp_set_formatter_tag_functions fmt {
        print_open_tag = ignore; print_close_tag = ignore;
        mark_open_tag; mark_close_tag };
      pp_set_mark_tags fmt true;
      fmt

    let image_buffer = create_ibuf ()
    let image_formatter = formatter_of_image_buffer image_buffer

    let pp_open_attribute_tag fmt attr = pp_open_tag fmt (A.to_tag attr)

    let kstrf ?(attr=A.empty) ?(w=1000000) k format =
      let m = ref 0 in
      let f1 _ () =
        m := pp_get_margin image_formatter ();
        pp_set_margin image_formatter w;
        pp_open_attribute_tag image_formatter attr
      and k _ =
        pp_print_flush image_formatter ();
        pp_set_margin image_formatter !m;
        reset_ibuf image_buffer |> k
      in kfprintf k image_formatter ("%a" ^^ format) f1 ()

    let strf ?attr ?w format = kstrf ?attr ?w (fun i -> i) format

    let attr attr f fmt a =
      pp_open_attribute_tag fmt attr; f fmt a; pp_close_tag fmt ()
  end

  let (kstrf, strf, pp_attr) = Fmt.(kstrf, strf, attr)
end

module Operation = struct

  type t =
    | Text of A.t * Text.t
    | Skip of int

  type ops = t list list

  let (@:) op ops = match (op, ops) with
    | (Skip 0, _) -> ops
    | (Skip _, []) -> []
    | (Skip m, Skip n :: ops) -> Skip (m + n) :: ops
    | _ -> op :: ops

  let rec scan x w row i k =
    let open I in match i with

    | Empty | Void _ -> Skip w @: k

    | Segment _ when row > 0 -> Skip w @: k
    | Segment (attr, text) ->
        let t  = Text.sub text x w in
        let w1 = Text.width t in
        let p  = if w > w1 then Skip (w - w1) @: k else k in
        if w1 > 0 then Text (attr, t) @: p else p

    | Hcompose ((i1, i2), _) ->
        let w1 = width i1
        and w2 = width i2 in
        if x >= w1 + w2 then Skip w @: k else
        if x >= w1 then scan (x - w1) w row i2 k else
        if x + w <= w1 then scan x w row i1 k else
          scan x (w1 - x) row i1 @@ scan 0 (w - w1 + x) row i2 @@ k

    | Vcompose ((i1, i2), _) ->
        let h1 = height i1
        and h2 = height i2 in
        if row >= h1 + h2 then Skip w @: k else
        if row >= h1 then scan x w (row - h1) i2 k else scan x w row i1 k

    | Zcompose ((i1, i2), _) ->
        let rec stitch x w i = function
          | [] -> scan x w row i []
          | (Text (_, t) as op)::ops as opss ->
              let w1 = Text.width t in
              if w1 >= w then opss else op :: stitch (x + w1) (w - w1) i ops
          | Skip w1::ops ->
              scan x w1 row i @@
                if w1 >= w then ops else stitch (x + w1) (w - w1) i ops
        in stitch x w i2 @@ scan x w row i1 @@ k

    | Hcrop ((i, left, _), (w1, _)) ->
        if x >= w1 then Skip w @: k else
        if x + w <= w1 then scan (x + left) w row i k else
          scan (x + left) (w1 - x) row i @@ Skip (w - w1 + x) @: k

    | Vcrop ((i, top, _), (_, h1)) ->
        if row < h1 then scan x w (top + row) i k else Skip w @: k

  let of_image (w, h) i =
    List.(0 -- pred h |> map (fun row -> scan 0 w row i []))

end

module Cap = struct

  type op = Buffer.t -> unit

  let get op = Buffer.on 8 op

  let (&) op1 op2 buf = op1 buf; op2 buf

  type t = {
    skip    : int -> op
  ; sgr     : A.t -> op
  ; newline : op
  ; clreol  : op
  ; cursvis : bool -> op
  ; cursat  : int -> int -> op
  ; cr      : op
  ; altscr  : bool -> op
  ; mouse   : bool -> op
  }

  let ((<|), (<.), (<!)) = Buffer.(add_string, add_char, add_decimal)

  let sts = [ ";1"; ";3"; ";4"; ";5"; ";7" ]

  let ansi = {
      skip    = (fun n b -> b <| "\x1b[0m"; Buffer.add_chars b ' ' n)
    ; newline = (fun b -> b <| "\x1bE")
    ; altscr  = (fun x b -> b <| if x then "\x1b[?1049h" else "\x1b[?1049l")
    ; cursat  = (fun w h b -> b <| "\x1b["; b <! w; b <. ';'; b <! h; b <. 'H')
    ; cr      = (fun b -> b <| "\x1b[1G")
    ; clreol  = (fun b -> b <| "\x1b[K")
    ; cursvis = (fun x b -> b <| if x then "\x1b[34h\x1b[?25h" else "\x1b[?25l")
    ; mouse   = (fun x b -> b <| if x then "\x1b[?1000;1002;1005;1015;1006h"
                                      else "\x1b[?1000;1002;1005;1015;1006l")
    ; sgr     =
      fun attr b ->
        b <| "\x1b[0";
        ( match attr.A.fg with
          | Some c when c < 8   -> b <. ';' ; b <! (c + 30)
          | Some c when c < 16  -> b <. ';' ; b <! (c + 82)
          | Some c              -> b <| ";38;5;" ; b <! c
          | None -> ());
        ( match attr.A.bg with
          | Some c when c < 8   -> b <. ';' ; b <! (c + 40)
          | Some c when c < 16  -> b <. ';' ; b <! (c + 92)
          | Some c              -> b <| ";48;5;" ; b <! c
          | None -> ());
        let rec stflags f xs = match (f, xs) with
          | (0, _)|(_, []) -> ()
          | (_, x::xs)     -> if f land 1 > 0 then b <| x; stflags (f lsr 1) xs
        in stflags attr.A.st sts;
        b <. 'm'
    }

  let no0 _     = ()
  and no1 _ _   = ()
  and no2 _ _ _ = ()

  let dumb = {
      skip    = (fun n b -> Buffer.add_chars b ' ' n)
    ; newline = (fun b -> b <| "\n")
    ; altscr  = no1
    ; cursat  = no2
    ; cr      = no0
    ; clreol  = no0
    ; cursvis = no1
    ; sgr     = no1
    ; mouse   = no1
    }

  let clear cap = cap.cr & cap.clreol
  let erase cap = cap.sgr A.empty & cap.clreol
end

module Render = struct

  let to_buffer buf cap dim img =
    let open Cap in
    let render_op = Operation.(function
      | Skip n      -> cap.skip n buf
      | Text (a, x) -> cap.sgr a buf; Text.to_buffer buf x
    ) in
    let rec render_line = function
      | []      -> erase cap buf
      | [op]    -> erase cap buf; render_op op
      | op::ops -> render_op op; render_line ops in
    let rec lines = function
      | []      -> ()
      | [ln]    -> render_line ln
      | ln::lns -> render_line ln; cap.newline buf; lines lns
    in
    lines (Operation.of_image dim img)

  let to_string cap dim i =
    Buffer.on I.(width i * height i * 2) (fun buf -> to_buffer buf cap dim i)

end

module Unescape = struct

  type special = [
    `Escape
  | `Enter
  | `Tab
  | `Backspace
  | `Insert
  | `Delete
  | `Home | `End
  | `Arrow of [ `Up | `Down | `Left | `Right ]
  | `Page of [ `Up | `Down ]
  | `Function of int
  ]

  type button = [ `Left | `Middle | `Right | `Scroll of [ `Up | `Down ] ]

  type mods = [ `Meta | `Ctrl | `Shift ] list

  type key = [ special | `Uchar of Uchar.t ] * mods

  type mouse = [ `Press of button | `Drag | `Release ] * (int * int) * mods

  type event = [ `Key of key | `Mouse of mouse ]

  type esc =
    C0    of char
  | C1    of char
  | SS2   of char
  | CSI   of string * int list * char
  | Esc_M of int * int * int
  | Uchar of int

  let csi =
    let open Option in
    let rec priv acc = function
      | x::xs when btw x 0x3c 0x3f -> priv (x::acc) xs
      | xs                         -> param (String.of_uchars_rev acc) None [] xs
    and param prv p ps = function
      | x::xs when btw x 0x30 0x39 -> param prv (Some (get 0 p * 10 + x - 0x30)) ps xs
      | 0x3b::xs                   -> param prv None (get 0 p :: ps) xs
      | xs                         -> code prv (List.rev (to_list p @ ps)) xs
    and code prv ps = function (* Conflate two classes because urxvt... *)
      | x::xs when btw x 0x20 0x2f || btw x 0x40 0x7e ->
          Some (CSI (prv, ps, (Char.chr x)), xs)
      | _ -> None in
    priv []

  let rec demux = let open Char in function
    | 0x1b::0x5b::0x4d::a::b::c::xs -> Esc_M (a, b, c) :: demux xs
    | (0x1b::(0x5b::xs) | 0x9b::xs) ->
        let (r, xs) = csi xs |> Option.get (C1 '\x5b', xs) in r :: demux xs
    | (0x1b::0x4f::x::xs | 0x8f::x::xs)
      when Ucharp.is_ascii x              -> SS2 (chr x) :: demux xs
    | 0x1b::x::xs when btw x 0x40 0x5f    -> C1 (chr x) :: demux xs
    | x::xs when btw x 0x80 0x9f          -> C1 (chr (x - 0x40)) :: demux xs
    | x::xs when btw x 0 0x1f || x = 0x7f -> C0 (chr x) :: demux xs
    | x::xs                               -> Uchar x :: demux xs
    | []                                  -> []

  let xtrm_mod_flags = function
    | 2 -> Some [`Shift]
    | 3 -> Some [`Meta]
    | 4 -> Some [`Shift; `Meta]
    | 5 -> Some [`Ctrl]
    | 6 -> Some [`Shift; `Ctrl]
    | 7 -> Some [`Meta; `Ctrl]
    | 8 -> Some [`Shift; `Meta; `Ctrl]
    | _ -> None

  let mods_xtrm = function
    | [1;p] -> xtrm_mod_flags p
    | []    -> Some []
    | _     -> None

  let mods_rxvt = function
    | '~' -> Some []
    | '$' -> Some [`Shift]
    | '^' -> Some [`Ctrl]
    | '@' -> Some [`Ctrl; `Shift]
    | _ -> None

  let mods_common ps code = match (ps, code) with
    | ([], '~')  -> Some []
    | ([], c)    -> mods_rxvt c
    | ([p], '~') -> xtrm_mod_flags p
    | _          -> None

  let mouse_p p =
    let btn = match p land 3 with
      | 0 when bit 6 p -> `Scroll `Up
      | 0              -> `Left
      | 1 when bit 6 p -> `Scroll `Down
      | 1              -> `Middle
      | 2 when bit 6 p -> `ALL (* `Scroll `Left *)
      | 2              -> `Right
      | 3 when bit 6 p -> `ALL (* `Scroll `Right *)
      | _              -> `ALL
    and drag = bit 5 p
    and mods =
      (if bit 3 p then [`Meta] else []) @
      (if bit 4 p then [`Ctrl] else [])
    in (btn, drag, mods)

  let key k mods = Some (`Key (k, mods))

  let event_of_control_code =
    let uchar u = `Uchar (Uchar.of_int u) in
    let open Option in function

    | Uchar u -> Some (`Key (uchar u, []))

    | C0 '\x1b'        -> key `Escape []
    | C0 ('\b'|'\x7f') -> key `Backspace []
    | C0 '\n'          -> key `Enter []
    | C0 '\t'          -> key `Tab []

    | C0 x -> key (uchar (Char.code x + 0x40)) [`Ctrl]
    | C1 x -> key (uchar (Char.code x)) [`Meta]

    | CSI ("",[],'Z') -> key `Tab [`Shift]

    | CSI ("",p,'A') -> mods_xtrm p >>= key (`Arrow `Up)
    | CSI ("",p,'B') -> mods_xtrm p >>= key (`Arrow `Down)
    | CSI ("",p,'C') -> mods_xtrm p >>= key (`Arrow `Right)
    | CSI ("",p,'D') -> mods_xtrm p >>= key (`Arrow `Left)

    | CSI ("",[],'a') -> key (`Arrow `Up) [`Shift]
    | CSI ("",[],'b') -> key (`Arrow `Down) [`Shift]
    | CSI ("",[],'c') -> key (`Arrow `Right) [`Shift]
    | CSI ("",[],'d') -> key (`Arrow `Left) [`Shift]
    | SS2 ('A'|'a') -> key (`Arrow `Up) [`Ctrl]
    | SS2 ('B'|'b') -> key (`Arrow `Down) [`Ctrl]
    | SS2 ('C'|'c') -> key (`Arrow `Right) [`Ctrl]
    | SS2 ('D'|'d') -> key (`Arrow `Left) [`Ctrl]

    | CSI ("",5::p,c) -> mods_common p c >>= key (`Page `Up)
    | CSI ("",6::p,c) -> mods_common p c >>= key (`Page `Down)

    | CSI ("",2::p,c) -> mods_common p c >>= key `Insert
    | CSI ("",3::p,c) -> mods_common p c >>= key `Delete

    | CSI ("",[4],'h') -> key `Insert []
    | CSI ("",[],'L')  -> key `Insert [`Ctrl]
    | CSI ("",[],'P')  -> key `Delete []
    | CSI ("",[],'M')  -> key `Delete [`Ctrl]

    | CSI ("",p,'H')   -> mods_xtrm p >>= key `Home
    | CSI ("",[7|1],c) -> mods_rxvt c >>= key `Home

    | CSI ("",p,'F')   -> mods_xtrm p >>= key `End
    | CSI ("",[8|4],c) -> mods_rxvt c >>= key `End
    | CSI ("",[],'J')  -> key `End [`Ctrl]

    | SS2 ('P'..'S' as c) -> key (`Function (Char.code c - 0x4f)) []

    | CSI ("",p,('P'..'S' as c)) ->
        mods_xtrm p >>= key (`Function (Char.code c - 0x4f))

    | CSI ("",k::p,c) when btw k 11 15 || btw k 17 21 || btw k 23 26 ->
        mods_common p c >>= key (`Function ((k - 10) - (k - 10) / 6))

    | CSI ("<",[p;x;y],('M'|'m' as c)) ->
        let (btn, drag, mods) = mouse_p p in
        ( match (c, btn, drag) with
          | ('M', (#button as b), false) -> Some (`Press b)
          | ('M', #button, true)         -> Some `Drag
          | ('m', #button, false)        -> Some `Release
          (* | ('M', `ALL   , true)         -> Some `Move *)
          | _                            -> None
        ) >>| fun e -> `Mouse (e, (x - 1, y - 1), mods)

    | CSI ("",[p;x;y],'M') | Esc_M (p,x,y) as evt ->
        let (x, y) = match evt with Esc_M _ -> x - 32, y - 32 | _ -> x, y
        and (btn, drag, mods) = mouse_p (p - 32) in
        ( match (btn, drag) with
          | (#button as b, false) -> Some (`Press b)
          | (#button     , true ) -> Some `Drag
          | (`ALL        , false) -> Some `Release
          (* | (`ALL        , true)  -> Some `Move *)
          | _                     -> None
        ) >>| fun e -> `Mouse (e, (x - 1, y - 1), mods)

    | CSI _ | SS2 _ -> None

  let rec events = function
    | C0 '\x1b' :: cc :: ccs ->
      ( match event_of_control_code cc with
        | Some (`Key (k, mods)) -> `Key (k, `Meta :: mods) :: events ccs
        | Some _                -> `Key (`Escape, []) :: events (cc::ccs)
        | None                  -> events ccs )
    | cc::ccs -> (event_of_control_code cc |> Option.to_list) @ events ccs
    | [] -> []

  let decode = events &. demux

  type t = (event list * bool) ref

  let create () = ref ([], false)

  let next t = match !t with
    | (#event as e::es, eof) -> t := (es, eof) ; e
    | ([], false) -> `Await
    | _           -> `End

  let list_of_utf8 buf i l =
    let f cs _ = function `Uchar c -> (Uchar.to_int c) :: cs | _ -> cs in
    String.sub0cp (Bytes.unsafe_to_string buf) i l
    |> Uutf.String.fold_utf_8 f [] |> List.rev

  let input t buf i l = t := match !t with
    | (es, false) when l > 0 -> (es @ (list_of_utf8 buf i l |> decode), false)
    | (es, _)                -> (es, true)

  let pending t = match !t with ([], false) -> false | _ -> true

end

module Tmachine = struct

  (* XXX This is sad. This should be a composable, stateless transducer. *)

  type t = {
    cap           : Cap.t
  ; frags         : string Queue.t
  ; mutable curs  : (int * int) option
  ; mutable dim   : (int * int)
  ; mutable image : I.t
  ; mutable dead  : bool
  }

  let emitv t xs =
    if t.dead then
      invalid_arg "Notty: use of released terminal"
    else Queue.addv t.frags xs

  let cursor cap = Cap.(function
    | None        -> cap.cursvis false
    | Some (w, h) -> cap.cursvis true & cap.cursat (max h 0 + 1) (max w 0 + 1)
    )

  let create ~mouse cap = {
      cap
    ; curs  = None
    ; dim   = (0, 0)
    ; image = I.empty
    ; dead  = false
    ; frags =
      Queue.singleton Cap.(
        get (cap.altscr true & cursor cap None & cap.mouse mouse)
      )
    }

  let release t =
    if t.dead then false else begin
      emitv t [Cap.(
        get (t.cap.altscr false & t.cap.cursvis true & t.cap.mouse false)
      )];
      t.dead <- true; true
    end

  let output t = Queue.(try `Output (take t.frags) with Empty -> `Await)

  let refresh t = emitv t [
      Cap.(get (cursor t.cap None & t.cap.Cap.cursat 1 1))
    ; Render.to_string t.cap t.dim t.image
    ; Cap.get (cursor t.cap t.curs)
    ]

  let set_size t dim = t.dim <- dim
  let image t image = t.image <- image; refresh t
  let cursor t curs = t.curs <- curs; emitv t [Cap.get (cursor t.cap curs)]

  let size t = t.dim
  let dead t = t.dead
end

type attr  = A.t
type image = I.t

module Infix = struct
  let ((<->), (<|>), (</>)) = I.((<->), (<|>), (</>))
  let (++) = A.(++)
end
