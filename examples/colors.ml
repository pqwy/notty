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
  "empty"       , empty
; "bold"        , st bold
; "faint"       , st faint
; "italic"      , st italic
; "underline"   , st underline
; "blink"       , st blink
; "reverse"     , st reverse
; "bold/italic" , st bold ++ st italic
; "faint/italic", st faint ++ st italic
; "rev/underln" , st underline ++ st reverse
; "bold/rev"    , st reverse ++ st bold
; "faint/rev"   , st reverse ++ st faint
]

let image w =
  let open List in
  let core16 =
    let c1  = map (fun (n, c) -> I.string A.(fg c) n) colors
    and c2  = map (fun (n, c) -> I.string A.(fg black ++ bg c) n) colors
    in I.(vcat c1 <|> void 1 0 <|> vcat c2)
  and attr =
    I.( styles |> map (fun (n, s) -> hpad 0 1 (string s n)) |> hcat) in
  let combine imgs =
    List.map I.(fun (n, i) -> string A.empty n <-> i <-> void 0 1) imgs
    |> I.vcat |> I.pad ~l:1 ~t:1 in
  combine [
    "System colors:",     core16;
    "Color cube, 6x6x6:", Images.c_cube_ix;
    "Grayscale ramp:",    Images.c_gray_ramp;
    "24bit:",             Images.c_rainbow (w - 2) 1;
    "Text styles:",       attr
  ]

let () = Notty_unix.output_image_size @@ fun (w, _) -> image w
