open Notty
open Common

let showkey = function
  | `Fn n  -> "F" ^ string_of_int n
  | `Up    -> "UP"
  | `Down  -> "DOWN"
  | `Left  -> "LEFT"
  | `Right -> "RIGHT"
  | `End   -> "END"
  | `Home  -> "HOME"
  | `Ins   -> "INSERT"
  | `Del   -> "DEL"
  | `Pg_dn -> "PAGE DOWN"
  | `Pg_up -> "PAGE UP"
  | `Bs    -> "BACKSPACE"
  | `Enter -> "ENTER"
  | `Tab   -> "TAB"

let () =
  let magenta = A.(lightmagenta @/ black @// empty)
  and blue    = A.(lightblue @/ black @// empty)
  and white   = A.(white @/ empty)
  in
  simpleterm ~s:[]
    ~step:(fun xs -> function
      | `Key `Enter -> None
      | x           -> Some (List.take 25 (x::xs)))
    ~image:(fun xs ->
      let msg = I.string white "Push keys, press enter to stop." 
      and ks =
        List.map (function
          | `Uchar c as u ->
              I.(string magenta (Printf.sprintf "u%04x " c)
                  <|> uchar blue u 1 1)
          | `Key k -> I.string magenta (showkey k))
        xs |> I.vcat in
      I.(pad ~left:1 ~top:1 (vframe ~align:`Top 25 ks <-> msg)))
