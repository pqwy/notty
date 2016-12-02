open Notty
open Notty_unix
open Common

let rec main t (x, y as pos) =
  let img =
    let u_check = Uchar.of_int 0x2713 in
    let dot = I.(uchar A.(bg lightred ++ fg black) u_check 1 1 |> pad ~l:x ~t:y)
    and txt = I.strf ~attr:A.(fg lightblack) "@(%d, %d)" x y in
    I.(txt </> dot) in
  Term.image t img;
  Term.cursor t (Some pos);
  match Term.event t with
  | `End | `Key (`Escape, []) -> ()
  | `Key (`Uchar u, [`Ctrl]) when Uchar.to_int u = 67 -> ()
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
