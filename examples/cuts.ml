(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates text cropping, particularly of grapheme clusters and wide
 * characters.
 *)
open Notty
open Notty_unix
open Common

let hpadwith attr c a b i =
  I.(char attr c a 1 <|> i <|> char attr c b 1)

let cuts i =
  let w = I.width i in
  List.(
    range 0 w |> map (fun a ->
      range 0 (w - a) |> map (fun b ->
        i |> I.hcrop a b |> hpadwith A.(fg lightblack) '.' a b
      ) |> I.vcat |> I.hpad 1 1
    ) |> I.hcat |> I.vpad 1 1
  )

let colors = A.[red; green; yellow; blue; magenta; cyan]

let patterns = [
  "desu"
; ".▪e\204\129●."
; "(茶‸茶‶)"
; "(⌐■_■)"
(* ; "¯\\(ツ)/¯" *)
(* ; "ಠ_ಠ" *)
(* ; "ಡ_ಡ" *)
(* ; "\xe0\xb2\xa0\x5f\xe0\xb1\x83" *)
(* ; "ತಎತ" *)
(* ; "ಥ_ಥ" *)
; "ᕕ( ᐛ )ᕗ"
(* ; "ᕙ(⇀‸↼‶)ᕗ" *)
(* ; "ᕦ(ò_óˇ)ᕤ" *)
(* ; "(╯ ︵╰ )" *)
(* ; "\x28\x20\xcd\xa1\xc2\xb0\x20\xcd\x9c\xca\x96\x20\xcd\xa1\xc2\xb0\x29" *)
]


let () =
  let open I in

  patterns |> List.map (fun s ->
    cuts (string A.(fg lightmagenta ++ bg lightblack) s)
  ) |> I.vcat |> eol |> output_image ;

  colors |> List.mapi (fun i c ->
    pad ~l:i ~t:i (
      string A.(fg black ++ bg c ++ st blink) "茶" <|>
      pad ~l:2 ~t:1
        (string A.(fg c ++ st blink) "PARTY!"))
  ) |> zcat |> pad ~l:2 ~t:2 ~b:2 |> output_image
