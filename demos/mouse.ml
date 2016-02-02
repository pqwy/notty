open Notty
open Common

let lnv = 0x2502
and lnh = 0x2500
and crs = 0x253c

let () =
  simpleterm ~s:(`Down, (1, 1), [])
    ~f:(fun s -> function
      | `Mouse ((`Press `LMB|`Drag), pos, mods) -> Some (`Drag, pos, mods)
      | `Mouse (`Release, pos, mods) -> Some (`Down, pos, [])
      | _ -> Some s)
    ~imgf:I.(fun (w, h) (st, (x, y), mods) ->
      let cross =
        let a  = match st with `Drag -> A.(fg lightgreen) | `Down -> A.(fg green) in
        (uchar a lnh (x - 1) 1 |> vpad (y - 1) 0) <|>
        (uchar a lnv 1 (y - 1) <-> uchar a crs 1 1 <-> uchar a lnv 1 (h - y)) <|>
        (uchar a lnh (w - x) 1 |> vpad (y - 1) 0)
        |> crop ~top:1 ~left:1 ~right:1
        |> hpad 1 1
        |> vlimit ~align:`Top (h - 1)
      and status =
        let a = A.(darkgray @/ bg black) in
        let fa m = if List.mem m mods then A.(lightgreen @/ bg black) else a in
        string A.empty "Use the mouse." <^>
        (hcat [ string a "["
              ; string (fa `Ctrl) "C"
              ; string (fa `Meta) "M"
              ; stringp a "] @(%03d, %03d)" x y ]
         |> hlimit ~align:`Right w)
      in cross <-> status
    )
