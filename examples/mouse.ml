(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates mouse input.
 *)
open Notty
open Common

let lnv = Uchar.of_int 0x2502
and lnh = Uchar.of_int 0x2500
and crs = Uchar.of_int 0x253c

let clip a b x = min b (max a x)

let () =
  simpleterm ~s:(`Down, (0, 0), [], 11)
    ~f:(fun (st, pos, mods, scr as s) -> function
      | `Mouse ((`Press `Left|`Drag), pos, mods) -> Some (`Drag, pos, mods, scr)
      | `Mouse (`Press (`Scroll s), _, _) ->
          Some (st, pos, mods, clip 0 23 (scr + match s with `Up -> 1 | _ -> -1))
      | `Mouse (`Release, pos, _) -> Some (`Down, pos, [], scr)
      | _ -> Some s)
    ~imgf:I.(fun (w, h) (st, (x, y), mods, scr) ->
      let cross =
        let a  = match st with `Drag -> A.(fg lightgreen) | `Down -> A.(fg green) in
        (uchar a lnh x 1 |> vpad y 0) <|>
        (uchar a lnv 1 y <-> uchar a crs 1 1 <-> uchar a lnv 1 (h - y)) <|>
        (uchar a lnh (w - x - 1) 1 |> vpad y 0)
        |> crop ~t:1 ~l:1 ~r:3
        |> hpad 1 1
        |> vsnap ~align:`Top (h - 1)
      and scroll =
        List.(range 0 scr |> rev |> map @@ fun level ->
          Images.dot A.(gray level)
        ) |> vcat |> vsnap ~align:`Bottom (h - 1)
      and status =
        let a = A.(fg lightblack ++ bg black) in
        let fa m = if List.mem m mods then A.(fg lightgreen ++ bg black) else a in
        string A.empty "Use the mouse." </>
        (hcat [ string a "["
              ; string (fa `Ctrl) "C"
              ; string (fa `Meta) "M"
              ; strf ~attr:a "] @(%03d, %03d)" x y ]
         |> hsnap ~align:`Right w)
      in (cross <|> scroll) <-> status
    )
