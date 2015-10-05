open Notty
open Common
open Lwt

module T = Notty_lwt.Terminal

let img s = I.(
  string A.empty (string_of_int s) <-> hpad 2 0 (Images.sierp A.magenta s)
)

let main () =
  let term = T.create () in
  let rec go s =
    T.image term (img s) >>= fun () ->
    Lwt_stream.next (T.input term) >>= function
      | e when e = ch 'q'   -> return_unit
      | `Key (`Up|`Left)    -> go (max 1 (s - 1))
      | `Key (`Down|`Right) -> go (min 10 (s + 1))
      | _                   -> go s
  in
  go 1

let () = Lwt_main.run (main ())
