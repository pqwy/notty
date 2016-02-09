open Notty
open Lwt.Infix

include Common

module T = Notty_lwt.Term

let simpleterm_lwt ~imgf ~f ~s =
  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false });
  let term = T.create () in
  let imgf (w, h) s =
    I.(string A.(fg lightblack) "[ESC quits.]" <-> imgf (w, h - 1) s) in
  let rec step e s = match e with
    | `Key (`Escape, []) | `Key (`Uchar 67, [`Ctrl]) ->
        T.release term >|= fun () -> s
    | `Resize dim -> T.image term (imgf dim s) >|= fun () -> s
    | #Unescape.event as e ->
        match f s e with
        | Some s -> T.image term (imgf (T.size term) s) >|= fun () -> s
        | None   -> T.release term >|= fun () -> s
  in
  ( T.image term (imgf (T.size term) s)
    >>= fun () -> Lwt_stream.fold_s step (T.events term) s )
  |> Lwt_main.run |> ignore
