open Notty
open Common

let () =
  simpleterm ~s:1
    ~step:(fun s -> function
      | `Key (`Uchar 113, _) -> None
      | `Key ((`Up|`Left), _) -> Some (max 1 (s - 1))
      | `Key ((`Down|`Right), _) -> Some (min 10 (s + 1))
      | _ -> Some s)
    ~image:I.(fun s ->
      string A.empty (string_of_int s) <->
      hpad 2 0 (Images.sierp A.magenta s)
    )
