(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty
open Common

let rec main t (x, y as pos) =
  let img =
    let dot = I.string A.(bg lightred ++ fg black) "✓" |> I.pad ~l:x ~t:y
    and txt = I.strf ~attr:A.(fg lightblack) "@(%d, %d)" x y in
    I.(txt </> dot) in
  Term.image t img;
  Term.cursor t (Some pos);
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) -> ()
  | `Resize _ -> main t pos
  | `Mouse ((`Press _ | `Drag), pos, _) -> main t pos
  | `Key (`Arrow d, _) ->
    ( main t @@ match d with
      | `Up    -> (x, y - 1)
      | `Down  -> (x, y + 1)
      | `Left  -> (x - 1, y)
      | `Right -> (x + 1, y) )
  | _ -> main t pos

let () = main (Term.create ()) (0, 1)
