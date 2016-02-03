open Notty
open Lwt.Infix

include Common

module T = Notty_lwt.Terminal

let simpleterm_lwt ~imgf ~f ~s =
  let term = T.create () in
  let imgf (w, h) s =
    I.(string A.(fg darkgray) "[ESC quits.]" <-> imgf (w, h - 1) s) in
  let rec step e s = match e with
    | `Key (`Escape, []) -> T.release term >|= fun () -> s
    | `Resize dim        -> T.image term (imgf dim s) >|= fun () -> s
    | #Unescape.event as e ->
        match f s e with
        | Some s -> T.image term (imgf (T.size term) s) >|= fun () -> s
        | None   -> T.release term >|= fun () -> s
  in
  let inputs = T.events term in
  ( T.image term (imgf (T.size term) s)
    >>= fun () -> Lwt_stream.fold_s step inputs s )
  |> Lwt_main.run |> ignore
