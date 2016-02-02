open Notty
open Common_lwt
open Lwt.Infix

let img s = I.(
  string A.empty (string_of_int s) <-> hpad 2 0 (Images.sierp A.magenta s)
)

let () =
  simpleterm_lwt ~s:1
    ~f:(fun s -> function
      | `Key (`Uchar 113, _) -> None
      | `Key ((`Up|`Left), _) -> Some (max 1 (s - 1))
      | `Key ((`Down|`Right), _) -> Some (min 10 (s + 1))
      | _ -> Some s)
    ~imgf:I.(fun (w, h) s ->
      string A.empty (string_of_int s) <->
      pad ~left:2 ~top:1 (Images.sierp A.magenta s)
    )
