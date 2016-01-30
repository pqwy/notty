
type uchar = int

let (&.) f g x = f (g x)

let btw (x : int) a b = a <= x && x <= b

let invalid_arg_s fmt = Printf.ksprintf invalid_arg fmt


let maccum ~empty ~append xs =
  let rec step = function
    | []  -> empty
    | [x] -> x
    | xs  -> step (accum xs)
  and accum = function
    | []       -> []
    | [a]      -> [a]
    | a::b::xs -> append a b :: accum xs
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
  let rec range a b = if a > b then [] else a :: range (a + 1) b

  module Set (E : sig type t val compare : t -> t -> int end) = struct

    let rec cons e = function
      | [] -> [e]
      | x::xt as xs ->
          match E.compare e x with
          | -1 -> x :: cons e xt
          |  1 -> e :: xs
          |  _ -> xs

    let rec union xs ys = match (xs, ys) with
      | (_, []) -> xs
      | ([], _) -> ys
      | (x::xt, y::yt) ->
          match E.compare x y with
          | -1 -> x :: union xt ys
          |  1 -> y :: union xs yt
          |  _ -> x :: union xt yt
  end
end

module Uchar = struct

  let is_ctrl  u = btw u 0x00 0x1f || btw u 0x7f 0x9f
  let is_ascii u = u = u land 0x7f
end

module Char = struct

  include Char

  let is_ctrl  c = Uchar.is_ctrl  (code c)
  let is_ascii c = Uchar.is_ascii (code c)
end

module String = struct

  include String

  let fold_left f s str =
    let acc = ref s in
    for i = 0 to length str - 1 do acc := f !acc str.[i] done;
    !acc

  let for_all f = fold_left (fun a c -> a && f c) true

  let find f str =
    let n = length str in
    let rec go f str i =
      if i < n then if f str.[i] then Some i else go f str (succ i) else None
    in go f str 0

  let of_uchars_rev = let open Bytes in function
    | []   -> ""
    | [u0] -> let b = create 1 in unsafe_set b 0 Char.(chr u0); b
    | ucs  ->
        let n = List.length ucs in
        let rec go bs i = function
          | []    -> bs
          | x::xs -> unsafe_set bs i (Char.chr x); go bs (pred i) xs in
        go (create n) (n - 1) ucs
end

module Int = struct

  type t = int

  let max (a : t) (b : t) = if a > b then a else b
  let min (a : t) (b : t) = if a < b then a else b
  let compare (a : t) (b : t) = compare a b
  let sign (a : t) = compare a 0
end

module Option = struct

  let map f = function Some x -> Some (f x) | _ -> None
  let get def = function Some x -> x | _ -> def
  let to_list = function Some x -> [x] | _ -> []
  let (>|=) a f = map f a
  let (>>=) a f = match a with Some x -> f x | _ -> None
end

module Utf8 = struct

  let with_encoder ?(hint=16) f =
    let buf = Buffer.create hint in
    let enc = Uutf.encoder `UTF_8 (`Buffer buf) in
    f enc; Uutf.encode enc `End |> ignore;
    Buffer.contents buf

  let of_uchars arr =
    let n = Array.length arr in
    with_encoder ~hint:n @@ fun enc ->
      for i = 0 to n - 1 do Uutf.encode enc (`Uchar arr.(i)) |> ignore done
end

module Text = struct

  open Rresult

  type t =
    | Ascii of string
    | Utf8  of string * int array * int

  let to_string = function
    | Utf8 (s, _, _) -> s
    | Ascii s        -> s

  let width = function
    | Utf8 (_, _, w) -> w
    | Ascii s        -> String.length s

  let empty = Ascii ""

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
      | `Uchar u when Uchar.is_ctrl u -> Error (`Control u)
      | `Uchar u -> go is (w + Uucp.Break.tty_width_hint u) i `Await
      | `End -> Ok is
    in
    go [0] 0 0 `Await >>| (Array.of_list &. List.rev)

  let dead = ' '

  let sub t x w =
    let open Int in
    let w1 = width t in
    if w = 0 || x >= w1 then empty else
      let w = min w (w1 - x) in
      match t with
      | Ascii s -> Ascii (String.sub s x w)
      | Utf8 (s, ix, _) ->
          let (l1, i) = match ix.(x) with
            | -1 -> (true, ix.(x + 1) - 1) | i -> (false, i)
          and (l2, j) = match ix.(x + w) with
            | -1 -> (true, ix.(x + w - 1) + 1) | j -> (false, j) in
          let n = j - i in
          let s = String.init n @@ fun k ->
            if l1 && k = 0 || l2 && k = n - 1 then dead else s.[k + i]
          and ix = Array.init (w + 1) @@ fun k ->
            if k = 0 then 0 else if k = w then n else max (-1) (ix.(k + x) - 1)
          in Utf8 (s, ix, w)

  let (code, chr, is_ctrl, is_ascii) = Char.(code, chr, is_ctrl, is_ascii)

  let err_ctrl_uchar = invalid_arg_s "Notty: control char: 0x%02x (from: %s)"
  let err_malformed  = invalid_arg_s "Notty: malformed UTF-8: %s (from: %s)"

  let of_ascii str =
    match String.find is_ctrl str with
    | Some i -> err_ctrl_uchar (code str.[i]) str
    | None   -> Ascii str

  let of_unicode str =
    match graphemes ~encoding:`UTF_8 str with
    | Ok ix                  -> Utf8 (str, ix, Array.length ix - 1)
    | Error (`Malformed err) -> err_malformed err str
    | Error (`Control u)     -> err_ctrl_uchar u str

  let of_string str =
    if String.for_all is_ascii str then of_ascii str else of_unicode str

  let of_uchars = of_string &. Utf8.of_uchars

  let replicatec w c =
    let str = String.make w c in
    if is_ctrl c then err_ctrl_uchar (code c) str else Ascii str

  let replicateu w u =
    if Uchar.is_ascii u then replicatec w (chr u)
    else of_unicode @@ Utf8.with_encoder ~hint:w @@ fun enc ->
      for _ = 1 to w do Uutf.encode enc (`Uchar u) |> ignore done
end

module A = struct

  module S = List.Set (Char)

  type color = int

  type style = char

  type t = {
    fg : color option
  ; bg : color option
  ; st : style list
  }

  let black        = 0
  let red          = 1
  let green        = 2
  let yellow       = 3
  let blue         = 4
  let magenta      = 5
  let cyan         = 6
  let lightgray    = 7
  let darkgray     = 8
  let lightred     = 9
  let lightgreen   = 10
  let lightyellow  = 11
  let lightblue    = 12
  let lightmagenta = 13
  let lightcyan    = 14
  let white        = 15

  let rgb ~r ~g ~b =
    if r < 0 || g < 0 || b < 0 || r > 5 || g > 5 || b > 5 then
      invalid_arg "Notty.A.rgb: a component outside of range [0, 5]"
    else r * 36 + g * 6 + b + 16

  let gray ~level =
    if level < 0 || level > 23 then
      invalid_arg "Notty.A.gray: level outside of range [0, 23]"
    else level + 232


  let bold      = '1'
  let italic    = '3'
  let underline = '4'
  let blink     = '5'
  let reverse   = '7'


  let empty = { fg = None; bg = None; st = []}

  let (&) a1 a2 = {
    fg = (match a2.fg with None -> a1.fg | x -> x)
  ; bg = (match a2.bg with None -> a1.bg | x -> x)
  ; st = S.union a1.st a2.st
  }

  let (@/)  f a = { a with fg = Some f }
  let (@//) b a = { a with bg = Some b }
  let (@+)  s a = { a with st = S.cons s a.st }

  let fg f = f @/ empty
  let bg b = b @// empty
  let st s = s @+ empty
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

  let (<^>) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = Int.max (width t1) (width t2)
        and h = Int.max (height t1) (height t2) in
        Zcompose ((t1, t2), (w, h))

  let void w h =
    if w < 0 || h < 0 || (w = 0 && h = 0) then Empty else Void (w, h)

  let lincrop crop void (++) init fini t =
    match Int.(sign init, sign fini) with
    | (1, 1) -> crop init fini t
    | (1, _) -> crop init 0 t ++ void (-fini)
    | (_, 1) -> void (-init) ++ crop 0 fini t
    | _      -> void (-init) ++ t ++ void (-fini)

  let hcrop =
    let ctor left right t =
      let w = width t - left - right in
      if w > 0 then Hcrop ((t, left, right), (w, height t)) else Empty
    in lincrop ctor (fun w -> void w 0) (<|>)

  let vcrop =
    let ctor top bottom t =
      let h = height t - top - bottom in
      if h > 0 then Vcrop ((t, top, bottom), (width t, h)) else Empty
    in lincrop ctor (void 0) (<->)

  let crop ?(left=0) ?(right=0) ?(top=0) ?(bottom=0) t =
    let t = if left <> 0 || right <> 0 then hcrop left right t else t in
    if top <> 0 || bottom <> 0 then vcrop top bottom t else t

  let hpad left right t = hcrop (-left) (-right) t

  let vpad top bottom t = vcrop (-top) (-bottom) t

  let pad ?(left=0) ?(right=0) ?(top=0) ?(bottom=0) t =
    crop ~left:(-left) ~right:(-right) ~top:(-top) ~bottom:(-bottom) t

  let hcat = maccum ~empty ~append:(<|>)

  let vcat = maccum ~empty ~append:(<->)

(*   let zcat = maccum ~empty ~append:(<^>) *)
  let zcat xs = List.fold_right (<^>) xs empty

  let tile w h i =
    List.(replicate h (replicate w i |> hcat) |> vcat)

  let text attr t = Segment (attr, t)

  let string attr s = text attr (Text.of_string s)

  let uchars attr a = text attr (Text.of_uchars a)

  let char attr c w h =
    tile 1 h (text attr (Text.replicatec w c))

  let uchar attr (`Uchar u) w h =
    tile 1 h (text attr (Text.replicateu w u))

  let hframe ?(align=`Middle) w t =
    let off = width t - w in match align with
      | `Left   -> hcrop 0 off t
      | `Right  -> hcrop off 0 t
      | `Middle -> let w1 = off / 2 in hcrop w1 (off - w1) t

  let vframe ?(align=`Middle) h t =
    let off = height t - h in match align with
      | `Top    -> vcrop 0 off t
      | `Bottom -> vcrop off 0 t
      | `Middle -> let h1 = off / 2 in vcrop h1 (off - h1) t

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
(*     | (Text (_, t), _) when Text.width t = 0 -> ops *)
    | _ -> op :: ops
(*     | (Attr a1, (Text _ as t)::(Attr a2)::xs)
        when Attr.equal a1 a2 -> op::t::xs |+ XXX ? +|
    | (Attr _ , []          ) -> []
|+     | (Attr a1, Attr a2::ops) -> Attr Attr.(a1 ++ a2) :: ops +|
    | (Skip _ , []          ) -> []
    | (Skip m , Skip n ::ops) -> Skip (m + n) :: ops
    | _                       -> op :: ops *)

(*   let (@:) op ops = op :: ops *)

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
    List.(range 0 (h - 1) |> map (fun row -> scan 0 w row i []))

end

module Cap = struct

  type op = Buffer.t -> unit

  let get op =
    let buf = Buffer.create 8 in
    op buf; Buffer.contents buf

  let (&) op1 op2 buf = op1 buf; op2 buf

  type t = {
    skip    : int -> op
  ; sgr     : A.t -> op
  ; newline : op
  ; clreol  : op
  ; cursvis : bool -> op
  ; cursat  : int -> int -> op
  ; altscr  : bool -> op
  ; mouse   : bool -> op
  }

  let i = string_of_int

  let (<|) = Buffer.add_string
  and (<.) = Buffer.add_char

  let csi op xs b =
    let rec wr b = function
      | []    -> ()
      | [x]   -> b <| x
      | x::xs -> b <| x; b <. ';'; wr b xs in
    b <| "\x1b["; wr b xs; b <. op

  let ansi = {
      skip    = (fun n b -> csi 'C' [i n] b)
    ; newline = (fun b -> b <| "\x1bE")
    ; altscr  = (fun x b -> b <| if x then "\x1b[?1049h" else "\x1b[?1049l")
    ; cursat  = (fun w h b -> csi 'H' [i w; i h] b)
    ; clreol  = (fun b -> b <| "\x1b[K")
    ; cursvis = (fun x b -> b <| if x then "\x1b[34h\x1b[?25h" else "\x1b[?25l")
    ; mouse   = (fun x b -> b <| if x then "\x1b[?1000;1002;1005;1015;1006h"
                                      else "\x1b[?1000;1002;1005;1015;1006l")
    ; sgr     =
      fun attr b ->
        b <| "\x1b[0";
        ( match attr.A.fg with
          | Some c when c < 8   -> b <. ';' ; b <| i (c + 30)
          | Some c when c < 16  -> b <. ';' ; b <| i (c + 82)
          | Some c              -> b <| ";38;5;" ; b <| i c
          | None -> ());
        ( match attr.A.bg with
          | Some c when c < 8   -> b <. ';' ; b <| i (c + 40)
          | Some c when c < 16  -> b <. ';' ; b <| i (c + 92)
          | Some c              -> b <| ";48;5;" ; b <| i c
          | None -> ());
        List.iter (fun s -> b <. ';' ; b <. s) attr.A.st;
        b <. 'm'
    }

  let no0 _ = ()
  and no1 _ _ = ()
  and no2 _ _ _ = ()

  let dumb = {
      skip    = (fun n b -> for _ = 1 to n do b <. ' ' done)
    ; newline = (fun b -> b <| "\n")
    ; altscr  = no1
    ; cursat  = no2
    ; clreol  = no0
    ; cursvis = no1
    ; sgr     = no1
    ; mouse   = no1
    }

end

module Render = struct

  let to_buffer cap dim img buf =
    let open Cap in
    let render_op = Operation.(function
      | Skip n      -> cap.skip n buf
      | Text (a, x) -> cap.sgr a buf; Buffer.add_string buf Text.(to_string x)
    ) in
    let render_line line =
      cap.clreol buf;
      line |> List.iter render_op;
      cap.sgr A.empty buf
    in
    let rec lines = function
      | []      -> ()
      | [ln]    -> render_line ln
      | ln::lns -> render_line ln; cap.newline buf; lines lns
    in
    lines (Operation.of_image dim img)

  let to_string cap dim i =
    let buf = Buffer.create I.(width i * height i * 2) in
    to_buffer cap dim i buf;
    Buffer.contents buf

end

module Unescape = struct

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

  type mods = [ `Meta | `Ctrl ] list

  type button = [ `LMB | `MMB | `RMB | `Scroll_up | `Scroll_dn ]

  type event = [
  | `Key   of [ special | `Uchar of uchar ] * mods
  | `Mouse of [ `Press of button | `Drag | `Release ] * (int * int) * mods
  ]

  type esc =
    C0    of char
  | C1    of char
  | SS2   of char
  | CSI   of string * int list * string
  | Esc_M of int * int * int
  | Uchar of int


  let csi =
    let open Option in
    let r_str = String.of_uchars_rev in
    let rec priv acc = function
      | x::xs when btw x 0x3c 0x3f -> priv (x::acc) xs
      | xs                         -> param (r_str acc) None [] xs
    and param prv p ps = function
      | x::xs when btw x 0x30 0x39 -> param prv (Some (get 0 p * 10 + x - 0x30)) ps xs
      | 0x3b::xs                   -> param prv None (get 0 p :: ps) xs
      | xs                         -> code prv (List.rev (to_list p @ ps)) [] xs
    and code prv ps acc = function
      | x::xs when btw x 0x20 0x2f -> code prv ps (x::acc) xs
      | x::xs when btw x 0x40 0x7e -> Some (CSI (prv, ps, r_str (x::acc)), xs)
      | _ -> None in
    priv []

  let rec demux = let open Char in function
    | 0x1b::0x5b::0x4d::a::b::c::xs -> Esc_M (a, b, c) :: demux xs
    | 0x1b::(0x5b::xs as xs0) ->
        let (r, xs) = csi xs |> Option.get (C0 '\x1b', xs0) in r :: demux xs
    | (0x1b::0x4f::x::xs | 0x8f::x::xs)
      when Uchar.is_ascii x               -> SS2 (chr x) :: demux xs
    | 0x1b::x::xs when btw x 0x40 0x5f    -> C1 (chr x) :: demux xs
    | x::xs when btw x 0x80 0x9f          -> C1 (chr (x - 0x40)) :: demux xs
    | x::xs when btw x 0 0x1f || x = 0x7f -> C0 (chr x) :: demux xs
    | x::xs                               -> Uchar x :: demux xs
    | []                                  -> []


  let mods_xtrm = function
    | []    -> Some []
    | [1;3] -> Some [`Meta]
    | [1;5] -> Some [`Ctrl]
    | [1;7] -> Some [`Meta; `Ctrl]
    | _     -> None

  let mods_rxvt = function "~" -> Some [] | "^" -> Some [`Ctrl] | _ -> None

  let mods_ab ps_tl code = match (ps_tl, code) with
    | ([] , "~")             -> Some []
    | ([5], "~") | ([], "^") -> Some [`Ctrl]
    | ([3], "~")             -> Some [`Meta]
    | ([7], "~")             -> Some [`Meta; `Ctrl]
    | _                      -> None

  let bit n b = b land (1 lsl n) > 0

  let mouse_p p =
    let btn = match p land 3 with
      | 0 when bit 6 p -> `Scroll_up
      | 0              -> `LMB
      | 1 when bit 6 p -> `Scroll_dn
      | 1              -> `MMB
      | 2 when bit 6 p -> `Scroll_left
      | 2              -> `RMB
      | 3 when bit 6 p -> `Scroll_right
      | _              -> `ALL
    and drag = bit 5 p
    and mods =
      (if bit 3 p then [`Meta] else []) @ (if bit 4 p then [`Ctrl] else [])
    in (btn, drag, mods)

  let key k mods = Some (`Key (k, mods))

  let event_of_control_code =
    let open Option in function

    | Uchar u -> Some (`Key (`Uchar u, []))

    | C0 '\x1b'        -> key `Escape []
    | C0 ('\b'|'\x7f') -> key `Backspace []
    | C0 '\n'          -> key `Enter []
    | C0 '\t'          -> key `Tab []

    | C0 x -> key (`Uchar (Char.code x + 0x40)) [`Ctrl]
    | C1 x -> key (`Uchar (Char.code x)) [`Meta]

    | CSI ("",p,"A") -> mods_xtrm p >>= key `Up
    | CSI ("",p,"B") -> mods_xtrm p >>= key `Down
    | CSI ("",p,"C") -> mods_xtrm p >>= key `Right
    | CSI ("",p,"D") -> mods_xtrm p >>= key `Left
    | SS2 ('A'|'a')  -> key `Up [`Ctrl]
    | SS2 ('B'|'b')  -> key `Down [`Ctrl]
    | SS2 ('C'|'c')  -> key `Right [`Ctrl]
    | SS2 ('D'|'d')  -> key `Left [`Ctrl]

    | CSI ("",5::p,("~"|"^" as c)) -> mods_ab p c >>= key `Pg_up
    | CSI ("",6::p,("~"|"^" as c)) -> mods_ab p c >>= key `Pg_dn

    | CSI ("",2::p,("~"|"^" as c)) -> mods_ab p c >>= key `Insert
    | CSI ("",3::p,("~"|"^" as c)) -> mods_ab p c >>= key `Delete

    | CSI ("",[4],"h") -> key `Insert []
    | CSI ("",[],"L")  -> key `Insert [`Ctrl]
    | CSI ("",[],"P")  -> key `Delete []
    | CSI ("",[],"M")  -> key `Delete [`Ctrl]

    | CSI ("",p,"H")                -> mods_xtrm p >>= key `Home
    | CSI ("",[7|1],("~"|"^" as c)) -> mods_rxvt c >>= key `Home

    | CSI ("",p,"F")                -> mods_xtrm p >>= key `End
    | CSI ("",[8|4],("~"|"^" as c)) -> mods_rxvt c >>= key `End
    | CSI ("",[],"J")               -> key `End [`Ctrl]

    | SS2 ('P'|'Q'|'R'|'S' as c) -> key (`Fn (Char.code c - 0x4f)) []

    | CSI ("",p,("P"|"Q"|"R"|"S" as c)) ->
        mods_xtrm p >>= key (`Fn (Char.code c.[0] - 0x4f))

    | CSI ("",k::p,("~"|"^" as c))
      when btw k 11 15 || btw k 17 21 || btw k 23 24 ->
        mods_ab p c >>= key (`Fn ((k - 10) - (k - 10) / 6))

    | CSI ("<",[p;x;y],("M"|"m" as c)) ->
        let (btn, drag, mods) = mouse_p p in
        ( match (c, btn, drag) with
          | ("M", (#button as b), false) -> Some (`Press b)
          | ("M", #button, true)         -> Some `Drag
          | ("m", #button, false)        -> Some `Release
          (* | ("M", `ALL   , true)         -> Some `Move *)
          | _                            -> None
        ) >|= fun e -> (`Mouse (e, (x, y), mods))

    | CSI ("",[p;x;y],"M") | Esc_M (p,x,y) ->
        let (btn, drag, mods) = mouse_p (p - 32) in
        ( match (btn, drag) with
          | (#button as b, false) -> Some (`Press b)
          | (#button     , true ) -> Some `Drag
          | (`ALL        , false) -> Some `Release
          (* | (`ALL        , true)  -> Some `Move *)
          | _                     -> None
        ) >|= fun e -> `Mouse (e, (x, y), mods)

    | CSI _ | SS2 _ -> None

  let rec events = function
    | C0 '\x1b' :: cc :: ccs ->
      ( match event_of_control_code cc with
        | Some (`Key (k, mods)) -> `Key (k, `Meta :: mods) :: events ccs
        | Some _                -> `Key (`Escape, []) :: events (cc::ccs)
        | None                  -> `Key (`Escape, []) :: events ccs )
    | cc::ccs -> (event_of_control_code cc |> Option.to_list) @ events ccs
    | [] -> []

  let decode xs = xs |> demux |> events


  type t = event list ref

  let create () = ref []

  (* XXX `End *)
  let next t = match !t with
    | (#event as e)::es -> t := es ; e
    | [] -> `Await

  let list_of_utf8 str =
    let f cs _ = function `Uchar c -> c::cs | _ -> cs in
    str |> Uutf.String.fold_utf_8 f [] |> List.rev

  let input t s i j =
    let s = String.(if i > 0 || j < length s then sub s i j else s)
    in t := !t @ (s |> list_of_utf8 |> decode)

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

  let emitv t xs = Queue.addv t.frags xs

  let cursor cap = Cap.(function
    | None        -> cap.cursvis false
    | Some (w, h) -> cap.cursvis true & cap.cursat h w
    )

  let create cap = {
      cap
    ; curs  = None
    ; dim   = (0, 0)
    ; image = I.empty
    ; dead  = false
    ; frags =
      Queue.singleton Cap.(
        get (cap.altscr true & cursor cap None & cap.mouse true)
      )
    }

  let output t = Queue.(try `Output (take t.frags) with Empty -> `Await)

  let refresh t = emitv t [
      Cap.(get (cursor t.cap None & t.cap.Cap.cursat 1 1))
    ; Render.to_string t.cap t.dim t.image
    ; Cap.get (cursor t.cap t.curs)
  ]

  let set_size t dim = t.dim <- dim

  let resize t dim = t.dim <- dim; refresh t

  let cursor t curs = t.curs <- curs; emitv t [Cap.get (cursor t.cap curs)]

  let image t image = t.image <- image; refresh t

  let finish t =
    if t.dead then false else begin
      emitv t [ Cap.(
        get (t.cap.altscr false & t.cap.cursvis true & t.cap.mouse false)
      )];
      t.dead <- true;
      true
    end

  let size t = t.dim

end

type attr  = A.t
type image = I.t
