(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates input parsing.
 *)
open Notty
open Common

let pps = Format.pp_print_string
let ppi = Format.pp_print_int

let pp_special fmt = function
  | `Escape       -> pps fmt "ESCAPE"
  | `Enter        -> pps fmt "ENTER"
  | `Tab          -> pps fmt "TAB"
  | `Backspace    -> pps fmt "BACKSPACE"
  | `Arrow `Up    -> pps fmt "UP"
  | `Arrow `Down  -> pps fmt "DOWN"
  | `Arrow `Left  -> pps fmt "LEFT"
  | `Arrow `Right -> pps fmt "RIGHT"
  | `Page `Up     -> pps fmt "PAGE UP"
  | `Page `Down   -> pps fmt "PAGE DOWN"
  | `Home         -> pps fmt "HOME"
  | `End          -> pps fmt "END"
  | `Insert       -> pps fmt "INSERT"
  | `Delete       -> pps fmt "DELETE"
  | `Function n   -> pps fmt "FN"; ppi fmt n

let pp_mods fmt = function
  | [] -> ()
  | ms -> ms |> List.iter (fun m ->
      pps fmt @@ match m with `Meta -> "M" | `Ctrl -> "C" | `Shift -> "S"
    )

let pp_mouse fmt = function
  | `Release -> pps fmt "Release"
  | `Drag    -> pps fmt "Drag"
  | `Move    -> pps fmt "Move"
  | `Press k ->
      pps fmt "Press ";
      pps fmt @@ match k with
        | `Left         -> "Left"
        | `Middle       -> "Middle"
        | `Right        -> "Right"
        | `Scroll `Up   -> "Scroll Up"
        | `Scroll `Down -> "Scroll Down"

let pp_u ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u)

let () =
  let magenta = A.(fg lightmagenta ++ bg black)
  and green   = A.(fg lightgreen   ++ bg black)
  and blue    = A.(fg lightblue    ++ bg black) in
  let pp_mods  = I.pp_attr green pp_mods
  and pp_mouse = I.pp_attr blue pp_mouse in
  simpleterm ~s:[]
    ~f:(fun xs x -> Some (List.take 100 (x::xs)))
    ~imgf:(fun (_, h) xs ->
      let attr = magenta in
      let msg = I.string A.empty "Push keys."
      and ks = List.map (function
        | `Key ((`ASCII _ | `Uchar _) as c, mods) ->
            let u = Unescape.uchar c in
            I.(uchar blue u 1 1 <|> strf ~attr " %a %a" pp_u u pp_mods mods)
        | `Key (#Unescape.special as k, mods) ->
            I.strf ~attr "%a %a" pp_special k pp_mods mods
        | `Mouse (e, (x, y), mods) ->
            I.strf ~attr "MOUSE %a (%d, %d) %a" pp_mouse e x y pp_mods mods
        | `Paste e ->
            I.strf ~attr "PASTE %s" (if e = `Start then "START" else "END")
        ) xs |> I.vcat in
      I.(vsnap ~align:`Top (h - 3) ks <-> void 0 1 <-> msg |> pad ~l:1 ~t:1))
