(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates Lwt interaction.
 *)
open Notty
open Common_lwt

let img s = I.(
  string A.empty (string_of_int s) <-> hpad 2 0 (Images.sierp A.magenta s)
)

let () =
  simpleterm_lwt ~s:1
    ~f:(fun s -> function
      | `Key (`ASCII 'q', _) -> None
      | `Key (`Arrow a, _) ->
        ( match a with
          | `Up | `Left -> Some (max 1 (s - 1))
          | `Down | `Right -> Some (min 10 (s + 1)) )
      | _ -> Some s)
    ~imgf:I.(fun _ s ->
      string A.empty (string_of_int s) <->
      pad ~l:2 ~t:1 (Images.sierp A.magenta s)
    )
