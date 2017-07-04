(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty
open Lwt.Infix

include Common

module T = Notty_lwt.Term

let simpleterm_lwt ~imgf ~f ~s =
  let term = T.create () in
  let imgf (w, h) s =
    I.(string A.(fg lightblack) "[ESC quits.]" <-> imgf (w, h - 1) s) in
  let step e s = match e with
    | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
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


let timer = function
  | None   -> Lwt.wait () |> fst
  | Some t -> Lwt_unix.sleep t >|= fun _ -> `Timer

let event e = Lwt_stream.get (T.events e) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let simpleterm_lwt_timed ?delay ~f s0 =
  let term = T.create () in
  let rec loop (e, t) dim s =
    (e <?> t) >>= function
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
        Lwt.return_unit
    | `Resize dim as evt     -> invoke (event term, t) dim s evt
    | #Unescape.event as evt -> invoke (event term, t) dim s evt
    | `Timer as evt          -> invoke (e, timer delay) dim s evt
  and invoke es dim s e =
    match f dim s e with
    | `Continue s    -> loop es dim s
    | `Redraw (s, i) -> T.image term i >>= fun () -> loop es dim s
    | `Stop          -> Lwt.return_unit in
  let size = T.size term in
  loop (event term, timer delay) size s0
