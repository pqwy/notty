(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates edge-case behavior of functions that produce rectangle-like
 * things.
 *)
open Notty
open Common

let hdistribute ?align w imgs =
  let n = List.length imgs in
  I.(List.map (hsnap ?align (w / n)) imgs |> hcat)

let take w h i = I.(vsnap h i |> hsnap w)

let () =
  simpleterm ~s:(2, 2)
    ~f:(fun (w, h as s) -> function
        `Key (`Arrow `Left, _) -> Some (w - 1, h)
      | `Key (`Arrow `Right, _) -> Some (w + 1, h)
      | `Key (`Arrow `Up, _) -> Some (w, h - 1)
      | `Key (`Arrow `Down, _) -> Some (w, h + 1)
      | `Key (`ASCII '0', _) -> Some (0, 0)
      | _ -> Some s)
    ~imgf:I.(fun (ow, oh) (w, h) ->
      let (a1, a2, a3) = A.(fg lightmagenta, fg lightred, fg lightblue) in
      strf "Sizing edge behavior. Dim: (%d, %d)" w h <->
      ( hdistribute ow Images.[
          outline a1 (uchar a1 (Uchar.of_int 0x2022) w h)
        ; outline a2 (uchar a2 (Uchar.of_int 0x2022) 300 300 |> take w h)
        ; outline a3 (void w h)
        ] |> vsnap (oh - 4) )
      <->
      hdistribute ow [string a1 "char"; string a2 "crop"; string a3 "void"]
    )
