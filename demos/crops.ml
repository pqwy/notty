open Notty
open Common

let d2 a b c d x (w, h) =
  if x = a then (w - 1, h) else
  if x = b then (w + 1, h) else
  if x = c then (w, h - 1) else (w, h + 1)

let hdistribute ?align w imgs =
  let n = List.length imgs in
  I.(List.map (hlimit ?align (w / n)) imgs |> hcat)

let take w h i = I.(vlimit h i |> hlimit w)

let () =
  simpleterm ~s:(2, 2)
    ~f:(fun (w, h as s) -> function
        `Key (`Left, _)     -> Some (w - 1, h)
      | `Key (`Right, _)    -> Some (w + 1, h)
      | `Key (`Up, _)       -> Some (w, h - 1)
      | `Key (`Down, _)     -> Some (w, h + 1)
      | `Key (`Uchar 48, _) -> Some (0, 0)
      | _                   -> Some s)
    ~imgf:I.(fun (ow, oh) (w, h) ->
      let (a1, a2, a3) = A.(fg lightmagenta, fg lightred, fg lightblue) in
      stringp A.(fg darkgray) "Dim: (%d, %d)" w h <->
      ( hdistribute ow Images.[
          outline a1 (uchar a1 0x2022 w h) 
        ; outline a2 (uchar a2 0x2022 300 300 |> take w h)
        ; outline a3 (void w h)
        ] |> vlimit (oh - 4) )
      <->
      hdistribute ow [string a1 "char"; string a2 "crop"; string a3 "void"]
    )
