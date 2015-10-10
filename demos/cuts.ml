open Notty
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

let () =
  let open I in
  let s1 = "desu"
  and s2 = ".◾e\204\129●."
  and s3 = ".茶.茶." in

  [s1; s2; s3] |> List.map (fun s ->
    cuts (string A.(lightmagenta @/ bg darkgray) s)
  ) |> I.vcat |> print;

  colors |> List.mapi (fun i c ->
    pad ~left:i ~top:i (
      string A.(blink @+ black @/ bg c) "茶" <|>
      pad ~left:2 ~top:1
        (string A.(blink @+ fg c) "PARTY!"))
  ) |> zcat |> pad ~left:2 ~top:2 ~bottom:2 |> print
