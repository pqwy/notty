open Notty
open Notty_unix
open Common

let rec main t (x, y as pos) =
  let img =
    let dot = I.(uchar A.(bg lightred ++ fg black) 0x2713 1 1 |> pad ~l:x ~t:y)
    and txt = I.strf ~attr:A.(fg lightblack) "@(%d, %d)" x y in
    I.(txt </> dot) in
  Term.image t img;
  Term.cursor t (Some pos);
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`Uchar 67, [`Ctrl]) -> ()
  | `Resize _ -> main t pos
  | `Mouse ((`Press _ | `Drag), pos, _) -> main t pos
  | `Key (`Arrow d, _) ->
    ( main t @@ match d with
      | `Up    -> (x, y - 1)
      | `Down  -> (x, y + 1)
      | `Left  -> (x - 1, y)
      | `Right -> (x + 1, y) )
  | _ -> main t pos

let () =
  let t = Term.create () in
  main t (0, 1)
