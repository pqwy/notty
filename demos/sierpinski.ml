open Notty
open Common

let () =
  simpleterm ~s:1
    ~f:(fun s -> function
      | `Key (`Uchar 113, _) -> None
      | `Key ((`Up|`Left), _) -> Some (max 1 (s - 1))
      | `Key ((`Down|`Right), _) -> Some (min 10 (s + 1))
      | _ -> Some s)
    ~imgf:I.(fun (w, h) s ->
      string A.empty (string_of_int s) <->
      pad ~left:2 ~top:1 (Images.sierp A.magenta s)
    )
