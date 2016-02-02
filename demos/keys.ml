open Notty
open Common

let string_of_special = function
  | `Escape    -> "ESCAPE"
  | `Enter     -> "ENTER"
  | `Tab       -> "TAB"
  | `Backspace -> "BACKSPACE"
  | `Up        -> "UP"
  | `Down      -> "DOWN"
  | `Left      -> "LEFT"
  | `Right     -> "RIGHT"
  | `Pg_up     -> "PAGE UP"
  | `Pg_dn     -> "PAGE DOWN"
  | `Home      -> "HOME"
  | `End       -> "END"
  | `Insert    -> "INSERT"
  | `Delete    -> "DELETE"
  | `Fn n      -> "FN" ^ string_of_int n

let string_of_mods ms =
  match
    let f s = function `Meta -> s ^ "M" | `Ctrl -> s ^ "C" in
    List.fold_left f "" ms
  with "" -> "" | s -> "(" ^ s ^ ")"

let string_of_mouse_e = function
  | `Release -> "RELEASE"
  | `Drag    -> "DRAG"
  | `Move    -> "MOVE"
  | `Press k -> "PRESS " ^ match k with
      | `LMB -> "left"
      | `MMB -> "middle"
      | `RMB -> "right"
      | `Scroll_up -> "scroll up"
      | `Scroll_dn -> "scroll down"


let () =
  let magenta = A.(lightmagenta @/ black @// empty)
  and green   = A.(lightgreen @/ black @// empty)
  and blue    = A.(lightblue @/ black @// empty)
  in
  simpleterm ~s:[]
    ~f:(fun xs x -> Some (List.take 100 (x::xs)))
    ~imgf:(fun (_, h) xs ->
      let msg = I.string A.empty "Push keys."
      and ks =
        let s_mods m = I.(void 1 1 <|> string green (string_of_mods m)) in
        List.map (function
          | `Key (`Uchar u, mods) ->
              I.(stringp magenta "u%04x " u <|>
                 uchar blue u 1 1 <|> s_mods mods )
          | `Key (#Unescape.special as k, mods) ->
              I.( string magenta (string_of_special k) <|> s_mods mods )
          | `Mouse (e, (x, y), mods) ->
              I.(stringp magenta "MOUSE %s (%d, %d)" (string_of_mouse_e e) x y
                 <|> s_mods mods)
        ) xs |> I.vcat in
      I.(pad ~left:1 ~top:1
          (vlimit ~align:`Top (h - 3) ks <-> void 0 1 <-> msg)))
