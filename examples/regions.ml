(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* Demonstrates how to change attributes of parts of an image. *)

open Notty
open Notty.Infix
open Common

let htake x w i = let w0 = I.width i in I.hcrop x (w0 - x - w) i
let vtake y h i = let h0 = I.height i in I.vcrop y (h0 - y - h) i

let hedit x w f i =
  htake 0 x i <|> f (htake x w i) <|> htake (x + w) (I.width i - x - w) i

let vedit y h f i =
  vtake 0 y i <-> f (vtake y h i) <-> vtake (y + h) (I.height i - y - h) i

let edit x y w h f = hedit x w (vedit y h f)

let edit_a x y w h a = edit x y w h (I.attr a)

let i =
  I.char '.' 7 7 |>
  List.fold_right (fun (i, c) -> edit_a i i 3 3 A.(bg c))
    A.[(0, magenta); (1, green); (2, red); (3, cyan); (4, blue)]

let _ = Notty_unix.(i |> eol |> output_image)

let bracket ~attr i = I.char '[' 1 1 ~attr <|> i <|> I.char ']' 1 1 ~attr

let progress q w0 =
  let w = w0 - 2 in
  let k = min q 1. *. float w |> truncate in
  (I.strf " %.02f%% " q |> I.hsnap w) </>
  I.char '.' w 1 ~attr:A.(fg black)
    |> hedit 0 k (I.attr A.(bg lightwhite ++ fg black))
    |> bracket ~attr:A.(fg lightwhite)

open Notty_unix

let _ =
  let label = I.string "Downloading more RAM: " in
  show_cursor false;
  for x = 0 to 100 do
    label <|> progress (float x /. 100.) 30 |> output_image;
    move_cursor `Home;
    flush stdout;
    Unix.sleepf 0.01
  done;
  label <|> I.string "RAM DOWNLOADED." ~attr:A.(st blink) |>
    eol |> output_image;
  show_cursor true
