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
        i |> I.hcrop a b |> hpadwith A.(fg darkgray) '.' a b
      ) |> I.vcat |> I.hpad 1 1
    ) |> I.hcat |> I.vpad 1 1
  )

let colors = A.[red; green; yellow; blue; magenta; cyan]

let patterns = [
  "desu"
; ".◾e\204\129●."
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
(* ; "\x28\x20\xcd\xa1\xc2\xb0\x20\xcd\x9c\xca\x96\x20\xcd\xa1\xc2\xb0\x29" *)
]


let () =
  let open I in

  patterns |> List.map (fun s ->
    cuts (string A.(lightmagenta @/ bg darkgray) s)
  ) |> I.vcat |> print_image_nl ;

  colors |> List.mapi (fun i c ->
    pad ~left:i ~top:i (
      string A.(blink @+ black @/ bg c) "茶" <|>
      pad ~left:2 ~top:1
        (string A.(blink @+ fg c) "PARTY!"))
  ) |> zcat |> pad ~left:2 ~top:2 ~bottom:2 |> print_image_nl
