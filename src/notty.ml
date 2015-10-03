
type uchar = int

(** **)

let maccum ~empty ~append xs =
  let rec step = function
    | []  -> empty
    | [x] -> x
    | xs  -> step (accum xs)
  and accum = function
    | []       -> []
    | [a]      -> [a]
    | a::b::xs -> append a b :: accum xs
  in
  step xs

module Eq = struct

  let option ~eq a b = match (a, b) with
    | (None  , None  ) -> true
    | (Some x, Some y) -> eq x y
    | _                -> false

  let rec list ~eq xs ys = match (xs, ys) with
    | (x::xss, y::yss) -> eq x y && list ~eq xss yss
    | _                -> false
end

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

module String = struct
  
  include String

  let of_char c0 =
    let b = Bytes.create 1 in
    Bytes.(unsafe_set b 0 c0; to_string b)

  let of_char_2 c0 c1 =
    let b = Bytes.create 2 in
    Bytes.(unsafe_set b 0 c0; unsafe_set b 1 c1; to_string b)

  let of_char_3 c0 c1 c2 =
    let b = Bytes.create 3 in
    Bytes.(unsafe_set b 0 c0; unsafe_set b 1 c1; unsafe_set b 2 c2; to_string b)

  let of_uchar   u1       = of_char   (Char.chr u1)
  let of_uchar_2 u1 u2    = of_char_2 (Char.chr u1) (Char.chr u2)
  let of_uchar_3 u1 u2 u3 = of_char_3 (Char.chr u1) (Char.chr u2) (Char.chr u3)

  let of_uchars_rev = function
    | []         -> ""
    | [u0]       -> of_uchar   u0
    | [u1;u0]    -> of_uchar_2 u0 u1
    | [u2;u1;u0] -> of_uchar_3 u0 u1 u2
    | ucs ->
        let rec rlen n a = function
          | []    -> (n, a)
          | c::cs -> rlen (n + 1) (c::a) cs in
        let rec wr b i = function
          | []    -> b
          | c::cs -> Bytes.set b i (Char.chr c); wr b (i + 1) cs in
        let (n, ucs) = rlen 0 [] ucs in
        Bytes.to_string @@ wr (Bytes.create n) 0 ucs

end

module Int = struct

  type t = int

  let max (a : t) (b : t) = if a > b then a else b
  let compare (a : t) (b : t) = compare a b
  let sign (a : t) = compare a 0
  let equal (a : t) (b : t) = a = b
end

module Char = struct
  include Char
  let equal (a : t) (b : t) = a = b
end

module Text = struct

  type t =
    | Ascii of string
    | Utf8  of string * int array * int

  let graphemes ?encoding str =
    let dec = Uutf.decoder ?encoding (`String str)
    and seg = Uuseg.create `Grapheme_cluster in
    let rec pull acc =
      let i = Uutf.decoder_byte_count dec in
      match Uutf.decode dec with
      | `Await        -> assert false
      | `End          -> push acc i `End
      | `Malformed _  -> push acc i (`Uchar Uutf.u_rep)
      | `Uchar _ as u -> push acc i u
    and push acc i x =
      match Uuseg.add seg x with
      | `Boundary -> push (i::acc) i `Await
      | `Uchar _  -> push acc i `Await
      | `Await    -> pull acc
      | `End      -> acc
    in
    Array.of_list (List.rev (pull [])) (* XXX faster *)

  let to_string = function
    | Utf8 (s, _, _) -> s
    | Ascii s        -> s

  let width = function
    | Utf8 (_, _, w) -> w
    | Ascii s        -> String.length s

  let empty = Ascii ""

  let sub t off len =
    let w = width t in
    if off >= w then empty else
    let len = if off + len > w then w - off else len in
    match t with
    | Ascii s -> Ascii (String.sub s off len)
    | Utf8 (s, ix, _) ->
        let i   = ix.(off)
        and j   = ix.(off + len) in
        let s'  = String.sub s i (j - i)
        and ix' = Array.sub ix off (len + 1) in
        for k = 0 to len - 1 do ix'.(k) <- ix'.(k) - i done;
        Utf8 (s', ix', len)

  let is_control_u u = u < 0x20 || u = 0x7f
  let is_ascii_u   u = u < 0x80

  let is_control c = is_control_u (Char.code c)
  let is_ascii   c = is_ascii_u   (Char.code c)

  let of_unicode str =
    let ix = graphemes ~encoding:`UTF_8 str in
    Utf8 (str, ix, Array.length ix - 1)

  let is_ascii_s s =
    let rec go i =
      if i >= String.length s then true
      else is_ascii s.[i] && go (succ i) in
    go 0

  let sprn = Printf.sprintf

  let of_string s =
    for i = 0 to String.length s - 1 do
      let c = String.unsafe_get s i in
      if is_control c then
        invalid_arg @@ sprn "Text.of_string: control character: %c" c
    done;
    if is_ascii_s s then Ascii s else of_unicode s

  let with_encoder f =
    let buf = Buffer.create 16 in
    let enc = Uutf.encoder `UTF_8 (`Buffer buf) in
    f enc; ignore (Uutf.encode enc `End);
    Buffer.contents buf

  let of_uchars arr =
    let n = Array.length arr in
    for i = 0 to n - 1 do
      let c = Array.unsafe_get arr i in
      if is_control_u c then
        invalid_arg @@ sprn "Text.of_uchars: control character: u%04x" c
    done;
    of_unicode @@ with_encoder @@ fun enc ->
      for i = 0 to n - 1 do
        ignore (Uutf.encode enc (`Uchar (Array.unsafe_get arr i)))
      done

  let encode_repeat n u =
    with_encoder @@ fun enc ->
      let chr = `Uchar u in
      for _ = 1 to n do ignore (Uutf.encode enc chr) done

  let replicatec w c =
    Ascii (String.make w (if is_control c then ' ' else c))

  let replicateu w = function
    | u when is_control_u u || not (Uutf.is_uchar u) ->
        of_unicode (encode_repeat w Uutf.u_rep)
    | u when u >= 0x80 ->
        of_unicode (encode_repeat w u)
    | u -> replicatec w (Char.chr u)

end

module A = struct

  module IS = List.Set (Char)

  type style = char

  let bold      = '1'
  let italic    = '3'
  let underline = '4'
  let blink     = '5'
  let reverse   = '7'


  type color = int

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

  let to_index x = x
  let of_index x =
    if x < 0 || x > 255 then
      invalid_arg "Cc.A.of_index: index outside of range [0, 255]"
    else x

  let rgb ~r ~g ~b =
    if r < 0 || g < 0 || b < 0 || r > 5 || g > 5 || b > 5 then
      invalid_arg "Cc.A.rgb: a component outside of range [0, 5]"
    else r * 36 + g * 6 + b + 16

  let gray ~level =
    if level < 0 || level > 23 then
      invalid_arg "Cc.A.gray: level outside of range [0, 23]"
    else level + 232

  type t = {
    fg : color option;
    bg : color option;
    st : style list
  }

  let empty = { fg = None; bg = None; st = []}

  let (&) a1 a2 = {
    fg = (match a2.fg with None -> a1.fg | x -> x)
  ; bg = (match a2.bg with None -> a1.bg | x -> x)
  ; st = IS.union a1.st a2.st
  }

  let (@/)  f a = { a with fg = Some f }
  let (@//) b a = { a with bg = Some b }
  let (@+)  s a = { a with st = IS.cons s a.st }

  let fg f = f @/ empty
  let bg b = b @// empty
  let st s = s @+ empty

  let equal a1 a2 =
    let aeq a b = Eq.option ~eq:Int.equal a b
    and seq a b = Eq.list ~eq:Char.equal a b in
    aeq a1.fg a2.fg && aeq a1.bg a2.bg && seq a1.st a2.st
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
    | Space    of dim

  let width = function
    | Empty -> 0
    | Segment (_, text) -> Text.width text
    | Hcompose (_, (w, _)) -> w
    | Vcompose (_, (w, _)) -> w
    | Zcompose (_, (w, _)) -> w
    | Hcrop    (_, (w, _)) -> w
    | Vcrop    (_, (w, _)) -> w
    | Space        (w, _)  -> w

  let height = function
    | Empty -> 0
    | Segment _ -> 1
    | Hcompose (_, (_, h)) -> h
    | Vcompose (_, (_, h)) -> h
    | Zcompose (_, (_, h)) -> h
    | Hcrop    (_, (_, h)) -> h
    | Vcrop    (_, (_, h)) -> h
    | Space        (_, h)  -> h

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

  let space w h =
    if w < 0 || h < 0 || (w = 0 && h = 0) then Empty else Space (w, h)

  let lincrop crop space (++) init fini t =
    match Int.(sign init, sign fini) with
    | (1, 1) -> crop init fini t
    | (1, _) -> crop init 0 t ++ space (-fini)
    | (_, 1) -> space (-init) ++ crop 0 fini t
    | _      -> space (-init) ++ t ++ space (-fini)

  let hcrop =
    let ctor left right t =
      let w = width t - left - right in
      if w > 0 then Hcrop ((t, left, right), (w, height t)) else Empty
    in lincrop ctor (fun w -> space w 0) (<|>)

  let vcrop =
    let ctor top bottom t =
      let h = height t - top - bottom in
      if h > 0 then Vcrop ((t, top, bottom), (width t, h)) else Empty
    in lincrop ctor (space 0) (<->)

  let crop ?(left=0) ?(right=0) ?(top=0) ?(bottom=0) t =
    let t = if left <> 0 || right <> 0 then hcrop left right t else t in
    if top <> 0 || bottom <> 0 then vcrop top bottom t else t

  let hpad left right t = hcrop (-left) (-right) t

  let vpad top bottom t = vcrop (-top) (-bottom) t

  let pad ?(left=0) ?(right=0) ?(top=0) ?(bottom=0) t =
    crop ~left:(-left) ~right:(-right) ~top:(-top) ~bottom:(-bottom) t

  let hcat = maccum ~empty ~append:(<|>)

  let vcat = maccum ~empty ~append:(<->)

  let zcat = maccum ~empty ~append:(<^>)

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

    | Empty | Space _ -> Skip w @: k

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
    skip    : int -> op;
    sgr     : A.t -> op;
    newline : op;
    clreol  : op;
    cursvis : bool -> op;
    cursat  : int -> int -> op;
    altscr  : bool -> op
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

  let ansi =
    { skip    = (fun n b -> csi 'C' [i n] b)
    ; newline = (fun b -> b <| "\x1bE")
    ; altscr  = (fun x b -> b <| if x then "\x1b[?1049h" else "\x1b[?1049l")
    ; cursat  = (fun w h b -> csi 'H' [i w; i h] b)
    ; clreol  = (fun b -> b <| "\x1b[K")
    ; cursvis = (fun x b -> b <| if x then "\x1b[34h\x1b[?25h" else "\x1b[?25l")
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

  let dumb =
    { skip    = (fun n b -> for _ = 1 to n do b <. ' ' done)
    ; newline = (fun b -> b <| "\n")
    ; altscr  = no1
    ; cursat  = no2
    ; clreol  = no0
    ; cursvis = no1
    ; sgr     = no1
    }

end

module Render = struct

  let buffer_for img = Buffer.create I.(width img * height img)

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

  let to_string cap dim img =
    let buf = buffer_for img in
    to_buffer cap dim img buf;
    Buffer.contents buf

end

module Unescape = struct

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

  let key_of_control_code = function

(*     | `C0 _ as c0 -> Some c0 *)
    | `C0 '\b' -> Some `Bs
    | `C0 '\n' -> Some `Enter
    | `C0 '\t' -> Some `Tab

    | `Cseq "A" | `C1 "OA"  -> Some `Up
    | `Cseq "B" | `C1 "OB"  -> Some `Down
    | `Cseq "C" | `C1 "OC"  -> Some `Right
    | `Cseq "D" | `C1 "OD"  -> Some `Left

    | `Cseq "5~" -> Some `Pg_up
    | `Cseq "6~" -> Some `Pg_dn

    | `Cseq ("2~"|"4h") -> Some `Ins
    | `Cseq ("3~"|"P")  -> Some `Del

    | `Cseq ("1~"|"7~"|"H") | `C1 "OH" -> Some `Home
    | `Cseq ("4~"|"8~"|"F") | `C1 "OF" -> Some `End

(*     | `C1 "OM" -> Some `Enter *)

    | `Cseq "11~" | `C1 "OP" -> Some (`Fn 1)
    | `Cseq "12~" | `C1 "OQ" -> Some (`Fn 2)
    | `Cseq "13~" | `C1 "OR" -> Some (`Fn 3)
    | `Cseq "14~" | `C1 "OS" -> Some (`Fn 4)
    | `Cseq "15~"            -> Some (`Fn 5)
    | `Cseq "17~"            -> Some (`Fn 6)
    | `Cseq "18~"            -> Some (`Fn 7)
    | `Cseq "19~"            -> Some (`Fn 8)
    | `Cseq "20~"            -> Some (`Fn 9)
    | `Cseq "21~"            -> Some (`Fn 10)
    | `Cseq "23~"            -> Some (`Fn 11)
    | `Cseq "24~"            -> Some (`Fn 12)
    
    | _ -> None


  type ('a, 'b) xd = K of ('a -> ('a, 'b) xd) | Y of 'b * ('a, 'b) xd

  let fin0     = String.of_uchars_rev
  let fin c cs = fin0 (c::cs)

  let escapes =
    let rec s0 = K start
    and ok x = Y (x, s0)
    and err c cs = Y (`Malformed (fin0 cs), start c)
    and start = function
      | 0x1b             -> K esc
      | 0x7f             -> ok (`Esc (`C0 '\x08'))
      | c when c <= 0x1f -> ok (`Esc (`C0 Char.(chr c)))
      | c                -> ok (`Uchar c)
    and esc = function
      | c when 0x40 <= c && c <= 0x5f -> c1 c
      | c                             -> err c [0x1b]
    and c1 = function
      | 0x5b           -> K (csi true [])
      | 0x4e|0x4f as c -> K (fun x -> ok (`Esc (`C1 (fin x [c]))))
      | c              -> ok (`Esc (`C1 (fin c [])))
    and csi p cs = function
      | c when 0x20 <= c && c <= 0x2f      -> K (csi false (c::cs))
      | c when 0x30 <= c && c <= 0x3f && p -> K (csi true  (c::cs))
      | c when 0x40 <= c && c <= 0x7e      -> ok (`Esc (`Cseq (fin c cs)))
      | c                                  -> err c (cs @ [0x5b;0x1b])
    in s0

  type res = [
    | `Uchar     of uchar
    | `Esc       of esc
    | `Malformed of string
    | `Await
    | `End
  ]

  type t = {
    dec : Uutf.decoder;
    mutable state : (uchar, res) xd
  }

  let next t =
    let rec loop dec = function
      | Y (x, s) -> t.state <- s; x
      | K f as s -> match Uutf.decode dec with
          | `Malformed _         -> loop dec s
          | `Uchar c             -> loop dec (f c)
          | (`End | `Await as r) -> t.state <- s; r
    in loop t.dec t.state

  let rec next_k t =
    match next t with
    | `Uchar _ | `End | `Await as r -> r
    | `Malformed _ -> next_k t
    | `Esc e -> 
        match key_of_control_code e with
        | Some c -> `Key c
        | None   -> next_k t

  let create () =
    let dec = Uutf.decoder ~encoding:`UTF_8 `Manual in
    { dec ; state = escapes }

  let input t s i j = Uutf.Manual.src t.dec s i j

(*   let of_string s =
    let rec loop i es = match next i with
      | `Await | `End -> List.rev es
      | `Uchar _ | `Esc _ | `Malformed _ as e -> loop i (e::es) in
    let i = create () in
  input i s 0 (String.length s); loop i [] *)

end

module Tmachine = struct

  type t = {
    cap           : Cap.t;
    frags         : string Queue.t;
    mutable curs  : (int * int) option;
    mutable dim   : (int * int);
    mutable image : I.t;
    mutable dead  : bool
  }

  let emitv t xs = Queue.addv t.frags xs

  let cursor cap = Cap.(function
    | None        -> cap.cursvis false
    | Some (w, h) -> cap.cursvis true & cap.cursat h w)

  let create cap = {
      cap
    ; curs  = None
    ; dim   = (0, 0)
    ; image = I.empty
    ; frags = Queue.singleton Cap.(get (cap.altscr true & cursor cap None))
    ; dead  = false
    }

  let output t = Queue.(try `Output (take t.frags) with Empty -> `Await)

  let refresh t = emitv t [
      Cap.(get (cursor t.cap None & t.cap.Cap.cursat 1 1))
    ; Render.to_string t.cap t.dim t.image
    ; Cap.get (cursor t.cap t.curs)
  ]

  let resize t dim = t.dim <- dim; refresh t

  let cursor t curs = t.curs <- curs; emitv t [Cap.get (cursor t.cap curs)]

  let image t curs image = t.image <- image; t.curs <- curs; refresh t

  let finish t =
    if t.dead then false else begin
      emitv t [Cap.(get (t.cap.altscr false & t.cap.cursvis true))];
      t.dead <- true;
      true
    end

  let size t = t.dim

end

module IO_helpers = struct

  external c_winsize : Unix.file_descr -> int = "caml_cc_winsize" "noalloc"
  external winch_number : unit -> int = "caml_cc_winch_number" "noalloc"

  let winsize fd =
    match c_winsize fd with
    | 0  -> None
    | wh -> Some (wh lsr 16, wh land 0xff)

  let cap_for_fd fd =
    match (Sys.getenv "TERM", Unix.isatty fd) with
    | exception Not_found         -> Cap.dumb
    | ((""|"dumb"), _)|(_, false) -> Cap.dumb
    | _                           -> Cap.ansi

  let setup_tcattr fd =
    let open Unix in
    let tc = tcgetattr fd in
    tcsetattr fd TCSANOW { tc with c_icanon = false ; c_echo = false };
    `Revert (fun () -> tcsetattr fd TCSANOW tc)

  let set_winch_handler f =
    let signum = winch_number () in
    let old_hdl = Sys.(signal signum (Signal_handle (fun _ -> f ()))) in
    `Revert (fun () -> Sys.set_signal signum old_hdl)

end

module Terminal = struct

  open IO_helpers

  type event = [ `End | `Uchar of uchar | `Key of Unescape.key ]

  module Input = struct

    type t = {
      fd      : Unix.file_descr;
      flt     : Unescape.t;
      ibuf    : bytes;
      cleanup : unit -> unit
    }

    let create fd =
      let flt  = Unescape.create ()
      and ibuf = Bytes.create 64
      and `Revert cleanup = setup_tcattr fd in
      { fd; flt; ibuf; cleanup }

    let release t = t.cleanup ()

    let rec input t =
      match Unescape.next_k t.flt with
      | `End | `Uchar _ | `Key _ as r -> r
      | `Await ->
          let n = Unix.read t.fd t.ibuf 0 Bytes.(length t.ibuf) in
          Unescape.input t.flt t.ibuf 0 n; input t

  end

  type t = {
    output : out_channel;
    trm    : Tmachine.t;
    input  : Input.t;
    winch  : bool ref * (unit -> unit)
  }

  let hook_winch cond =
    let r = ref false in
    let f =
      if cond then
        let `Revert f = set_winch_handler (fun () -> r := true) in f
      else (fun () -> ()) in
    (r, f)

  let unwinch (_, f) = f ()

  let wdrain (r, _) = let w = !r in if w then r := false; w

  let rec write t =
    match Tmachine.output t.trm with
    | `Output s -> output_string t.output s; write t
    | `Await    -> flush t.output

  let release t =
    if Tmachine.finish t.trm then begin
      unwinch t.winch;
      Input.release t.input;
      write t
    end

  let size t =
    match winsize Unix.(descr_of_out_channel t.output) with
    | Some d -> d | _ -> (0, 0)

  let resize t dim = Tmachine.resize t.trm dim; write t

  let create ?(dispose=true) ?(winch=true) ?(input=Unix.stdin) ?(output=Unix.stdout) () =
    let trm   = Tmachine.create (cap_for_fd input)
    and input = Input.create input
    and och   = Unix.out_channel_of_descr output
    and winch = hook_winch winch in
    let t     = { output = och ; trm; input; winch } in
    if dispose then at_exit (fun () -> release t);
    resize t (size t);
    t

  let rec input_w t =
    if wdrain t.winch then `Winch (size t) else
    try Input.input t.input
    with Unix.Unix_error (Unix.EINTR, _, _) -> input_w t

  let rec input t =
    match input_w t with
    | `Winch dim -> resize t dim; input t
    | (`Uchar _ | `Key _ | `End) as r -> r

  let refresh t = Tmachine.refresh t.trm; write t
  let update t ?cursor image = Tmachine.image t.trm cursor image; write t

end

type attr  = A.t
type image = I.t

let output_image ?cap chan i =
  let fd = Unix.descr_of_out_channel chan in
  let cap = match cap with
    | Some cap -> cap
    | None     -> IO_helpers.cap_for_fd fd
  and dim = match IO_helpers.winsize fd with
    | Some (w, _) -> I.(w, height i)
    | None        -> I.(width i, height i) in
  let buf = Render.buffer_for i in
  Render.to_buffer cap dim i buf;
  Buffer.output_buffer chan buf

let print_image = output_image stdout
