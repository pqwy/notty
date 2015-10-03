open Notty
open Common

let rec sierp color = let open I in function
  | 1 -> pad ~right:1 @@ string A.(fg color) "◾"
  | n -> let ss = sierp color (pred n) in ss <-> (ss <|> ss)

let () =
  List.range 1 5
    |> List.map I.(fun n -> pad ~top:1 ~left:1 (sierp A.lightblue n))
    |> List.iter print;
  simpleterm ~s:1
    ~step:(fun s -> function
      | e when e = ch 'q'   -> None
      | `Key (`Up|`Left)    -> Some (max 1 (s - 1))
      | `Key (`Down|`Right) -> Some (min 10 (s + 1))
      | _                   -> Some s)
    ~image:I.(fun s ->
      string A.empty (string_of_int s) <-> pad ~left:2 (sierp A.magenta s)
    )
