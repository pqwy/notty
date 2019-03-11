(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates text attributes.
 *)
open Notty
open Common

let colors = A.[
  "black"        , black
; "red"          , red
; "green"        , green
; "yellow"       , yellow
; "blue"         , blue
; "magenta"      , magenta
; "cyan"         , cyan
; "white"        , white
; "lightblack"   , lightblack
; "lightred"     , lightred
; "lightgreen"   , lightgreen
; "lightyellow"  , lightyellow
; "lightblue"    , lightblue
; "lightmagenta" , lightmagenta
; "lightcyan"    , lightcyan
; "lightwhite"   , lightwhite
]

let styles = A.[
  "empty"      , empty
; "bold"       , st bold
; "italic"     , st italic
; "underline"  , st underline
; "blink"      , st blink
; "reverse"    , st reverse
; "bold/italic", st bold ++ st italic
; "rev/underln", st underline ++ st reverse
; "bold/rev"   , st reverse ++ st bold
]

let image w =
  let open List in
  let core16 =
    let c1 = colors |> map @@ fun (n, c) -> I.string n ~attr:A.(fg c)
    and c2 = colors |> map @@ fun (n, c) -> I.string n ~attr:A.(fg black ++ bg c)
    in I.(vcat c1 <|> void 1 0 <|> vcat c2)
  and sts =
    I.(styles |> map (fun (n, attr) -> string ~attr n |> hpad 0 1) |> hcat) in
  let combine imgs =
    List.map I.(fun (n, i) -> string n <-> i |> vpad 0 1) imgs |>
      I.vcat |> I.pad ~l:1 ~t:1 in
  combine [
    "System colors:",     core16;
    "Color cube, 6x6x6:", Images.c_cube_ix;
    "Grayscale ramp:",    Images.c_gray_ramp;
    "24bit:",             Images.c_rainbow (w - 2) 1;
    "Text styles:",       sts
  ]

let () = Notty_unix.output_image_size @@ fun (w, _) -> image w
