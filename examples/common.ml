(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty
open Notty.Infix

let pow n e = int_of_float (float n ** float e)

let rec (--) a b = if a > b then [] else a :: succ a -- b

module List = struct

  include List

  let rec replicate n a = if n < 1 then [] else a :: replicate (n - 1) a

  let rec intersperse a = function
    | [] | [_] as t -> t
    | x::xs         -> x :: a :: intersperse a xs

  let rec take n = function
    | x::xs when n > 0 -> x :: take (pred n) xs
    | _ -> []

  let rec splitat n = function
    | x::xs when n > 0 ->
        let (a, b) = splitat (pred n) xs in (x::a, b)
    | xs -> ([], xs)

  let rec chunks n xs =
    match splitat n xs with
    | (a, []) -> [a]
    | (a, b)  -> a :: chunks n b

  let rec zip xs ys = match (xs, ys) with
    | ([], _) | (_, []) -> []
    | (x::xs, y::ys) -> (x, y) :: zip xs ys

end

module String = struct

  include String

  let repeat n str =
    let b = Buffer.create 16 in
    for _ = 1 to n do Buffer.add_string b str done;
    Buffer.contents b
end

let tile w h i = I.tabulate w h (fun _ _ -> i)

(** A few images used in several places. *)
module Images = struct

  let i1 =
    I.(string ~attr:A.(fg lightblack) "omgbbq" <->
       string ~attr:A.(fg white ++ bg red) "@")
    <|> I.(string ~attr:A.(fg green) "xo" |> pad ~t:2)

  let i2 = I.(hpad 1 1 (hcrop 1 1 @@ tile 3 3 i1) <|> i1)

  let i3 = tile 5 5 i2

  let i4 =
    let i = I.(i3 <|> crop ~t:1 i3 <|> i3) in
    I.(crop ~l:1 i <-> crop ~r:1 i <-> crop ~b:2 i)

  let i5 = 0 -- 15 |> List.map (fun i -> I.pad ~t:i ~l:(i*2) i2)
                   |> I.zcat
                   |> tile 5 1

  let spc = I.string " "

  let c_gray_ramp = I.tabulate 24 1 @@ fun g _ -> I.attr A.(gray g |> bg) spc

  let c_cube_ix =
    I.tabulate 6 1 @@ fun r _ ->
      I.hpad 0 1 @@ I.tabulate 6 6 @@ fun b g ->
        I.attr A.(rgb ~r ~g ~b |> bg) spc

  let c_cube_rgb =
    let f x = [| 0x00; 0x5f; 0x87; 0xaf; 0xd7; 0xff |].(x) in
    I.tabulate 6 1 @@ fun r _ ->
      I.hpad 0 1 @@ I.tabulate 6 6 @@ fun b g ->
        let c = A.rgb_888 ~r:(f r) ~g:(f g) ~b:(f b) in
        I.attr A.(bg c) spc

  let c_rainbow w h =
    let pi2     = 2. *. 3.14159 in
    let pi2_3   = pi2 /. 3.
    and f t off = sin (t +. off) *. 128. +. 128. |> truncate in
    let color t = A.rgb_888 ~r:(f t (-.pi2_3)) ~g:(f t 0.) ~b:(f t pi2_3) in
    I.tabulate (w - 1) 1 @@ fun x _ ->
      let t = (pi2 *. float x /. float w) +. 3.7 in
      I.char ~attr:A.(bg (color t)) ' ' 1 h

  (* U+25CF BLACK CIRCLE *)
  let dot = I.string "●"
  (* U+25AA BLACK SMALL SQUARE *)
  let square = I.string "▪"

  let rec cantor = function
    | 0 -> square
    | n ->
        let sub = cantor (pred n) in
        I.hcat (List.replicate (pow 3 n) square) <->
        (sub <|> I.void (pow 3 (n - 1)) 0 <|> sub)

  let checker n m i =
    let w = I.width i in
    I.(tile (n/2) (m/2) (hpad 0 w i <-> hpad w 0 i))

  let checker1 = checker 20 20 (I.char ' ' 2 1) |> I.attr A.(bg magenta)

  let rec sierp n = I.(
    if n > 1 then
      let ss = sierp (pred n) in ss <-> (ss <|> ss)
    else hpad 1 0 square
  )

  let grid xxs = xxs |> List.map I.hcat |> I.vcat

  let outline i =
    let (w, h) = I.(width i, height i) in
    let chr x = I.uchar (Uchar.of_int x) 1 1
    and hbar  = I.uchar (Uchar.of_int 0x2500) w 1
    and vbar  = I.uchar (Uchar.of_int 0x2502) 1 h in
    let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
    let frame = (a <|> hbar <|> b) <->
                (vbar <|> I.void w h <|> vbar) <->
                (d <|> hbar <|> c) in
    frame </> I.pad ~t:1 ~l:1 i
end

let rect = I.string "▄"

let pxmatrix w h f =
  I.tabulate w h @@ fun x y ->
    let y = y * 2 in
    I.attr A.((f x y |> bg) ++ (f x (y + 1) |> fg)) rect

module Term = Notty_unix.Term

let simpleterm ~imgf ~f ~s =
  let term = Term.create () in
  let imgf (w, h) s =
    I.(string ~attr:A.(fg lightblack) "[ESC quits.]" <-> imgf (w, h - 1) s) in
  let rec go s =
    Term.image term (imgf (Term.size term) s);
    match Term.event term with
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) -> ()
    | `Resize _ -> go s
    | #Unescape.event as e ->
        match f s e with Some s -> go s | _ -> ()
  in go s
