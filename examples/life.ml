(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(*
 * Game of Life with some ZX spectrum kitsch.
 *)

let flip f a b = f b a

(** Live, **)

module Coord = struct
  type t = int * int
  let compare ((a, b) : t) (c, d) =
    match compare a c with 0 -> compare b d | r -> r
  let equal ((a, b) : t) (c, d) = a = c && b = d
end

module CSet = struct
  include Set.Make (Coord)
  let of_list = List.fold_left (flip add) empty
  let map f s = fold (fun x s -> add (f x) s) s empty
end

module CMap = struct
  include Map.Make (Coord)
  let preimg p m =
    fold (fun k v s -> if p v then CSet.add k s else s) m CSet.empty
end

let erem x y = (x mod y + y) mod y
let square (w, h) (a, b as ab) =
  if a < 0 || a >= w || b < 0 || b >= h then (-1, -1) else ab
let torus (w, h) (a, b) = (erem a w, erem b h)
let moebius (w, h) (a, b as ab) =
  if a < 0 || a >= w then (erem a w, h - b - 1) else ab

let neigh topo (a, b) = [
  (a-1, b); (a+1, b); (a-1, b-1); (a-1, b+1)
; (a, b-1); (a, b+1); (a+1, b-1); (a+1, b+1)
] |> List.map topo

let step topo life =
  let nlive pt =
    List.(neigh topo pt |> filter (flip CSet.mem life) |> length) in
  let f1 pt acc =
    pt :: neigh topo pt |> List.fold_left (fun acc -> function
      | (-1, -1) -> acc
      | pt when CMap.mem pt acc -> acc
      | pt ->
          let n = nlive pt in
          acc |> CMap.add pt
            (if n = 3 || (n = 2 && CSet.mem pt life) then 0 else 1)
    ) acc in
  CSet.fold f1 life CMap.empty |> CMap.preimg ((=) 0)

let glider = CSet.of_list [(2,1); (3,2); (1,3); (2,3); (3,3)]

(** ...render, **)

open Notty
open Notty.Infix

let dot = I.string A.(fg lightred) "●"

let background step (n, m) =
  let k = 24. *. sin (float (step + m + n) /. 10.) |> truncate in
  if k > 0 then I.string A.(fg (gray k)) "." else I.void 1 1

let render (w, h) step life =
  I.tabulate w (h - 1) (fun x y ->
    let pt = (x, y) in if CSet.mem pt life then dot else background step pt
  ) <->
  I.(strf ~attr:A.(fg lightblack) "[generation %04d]" step |>
      hsnap ~align:`Right w)

(** ...and interact. **)

open Lwt.Infix
open Notty_lwt

let timer () = Lwt_unix.sleep 0.1 >|= fun () -> `Timer
let event term = Lwt_stream.get (Term.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let rec loop term (e, t) (dim, n, life as st) =
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
      Lwt.return_unit
  | `Timer ->
      Term.image term (render dim n life) >>= fun () ->
        loop term (e, timer ())
          (dim, n + 1, step (torus dim) life)
  | `Mouse ((`Press `Left|`Drag), (x, y), _) ->
      loop term (event term, t)
        (dim, n, CSet.add (torus dim (x, y)) life)
  | `Resize dim ->
      let life = CSet.map (torus dim) life in
      Term.image term (render dim n life) >>= fun () ->
        loop term (event term, t) (dim, n, life)
  | _ -> loop term (event term, t) st

let main () =
  let t = Term.create () in
  loop t (event t, timer ()) (Term.size t, 0, glider)

let () = Lwt_main.run @@ main ()
