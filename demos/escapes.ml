open Notty
open Notty_unix
open Common

let fg c = A.(c @/ black @// empty)

let key_name = function
  | `Up    -> "UP"
  | `Down  -> "DOWN"
  | `Right -> "RIGHT"
  | `Left  -> "LEFT"
  | `Pg_up -> "PAGE UP"
  | `Pg_dn -> "PAGE DOWN"
  | `Ins   -> "INSERT"
  | `Del   -> "DELETE"
  | `Home  -> "HOME"
  | `End   -> "END"
  | `Fn n  -> "F" ^ string_of_int n
  | `Bs    -> "BACKSPACE"
  | `Enter -> "ENTER"
  | `Tab   -> "TAB"

let s = Printf.sprintf

let show_cc cc =
  let fmt fgc tag body =
    I.(string (fg fgc) tag <|> hpad 1 0 (string (fg A.white) body))
  in match cc with
  | `Uchar c      -> I.(string (fg A.white) (s "U+%04X" c))
  | `C0 c         -> fmt A.lightgreen "C0" (string_of_int (Char.code c))
  | `C1 cs        -> fmt A.lightblue "C1" cs
  | `CSI cs       -> fmt A.lightmagenta "CSI" cs
  | `Malformed cs -> fmt A.lightred "MALFORMED" (s "%S" cs)

let show_k = function
  | #Unescape.esc as e ->
    ( match Unescape.key_of_control_code e with
      | None   -> I.void 0 1
      | Some k -> I.(string (fg A.lightyellow) (key_name k)) )
  | `Uchar _ as c -> I.uchar (fg A.lightyellow) c 1 1
  | _ -> I.void 0 1

let image h xs =
  let xs = List.(take (h - 4) xs) in
  let open I in
  let stuff =
    let c1 = List.map show_cc xs |> vcat
    and c2 = List.map show_k xs |> vcat in
    c1 <|> hpad 2 0 c2 |> vframe ~align:`Top (h - 4)
  and text =
    vpad 1 0 (string A.(fg lightgray) "Press keys, Space TWICE to exit.")
  in stuff <-> text |> pad ~left:2 ~top:1

let () =
  let t = Terminal.create ()
  and b = Bytes.create 64
  and f = Unescape.create () in
  let rec go sp xs =
    let (_, h) = Terminal.size t in
    Terminal.image t (image h xs);
    match Unescape.next f with
    | `End -> ()
    | `Uchar 0x20 when sp -> ()
    | (#Unescape.esc|`Malformed _|`Uchar _) as x ->
        go (x = `Uchar 0x20) (List.take 100 (x::xs))
    | `Await ->
        match Unix.(read stdin) b 0 Bytes.(length b) with
        | exception Unix.Unix_error _ -> go sp xs
        | n -> Unescape.input f b 0 n; go sp xs
  in go false []
