(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(*
 * Elementary Cellular Automata
 *)
open Notty
open Notty.Infix
open Common_lwt

let flip f a b = f b a
let rec take n = function
  | x::xs when n > 0 -> x :: take (pred n) xs
  | _                -> []

let getd arr d i =
  if i < 0 || i >= Array.length arr then d else arr.(i)

let f ~rule a b c =
  if rule land (1 lsl (a lsl 2 + b lsl 1 + c)) > 0 then 1 else 0

let step ~rule w arr =
  let get = getd arr 0 in
  Array.init w @@ fun i ->
    f ~rule (get (i - 1)) (get i) (get (i + 1))

let dot  = I.char A.(bg lightwhite) ' ' 1 1
let void = I.void 1 1

let render ~rule ~h xss =
  let cons k = function
    | 0 -> I.void k 1
    | _ -> I.char A.(bg lightwhite) ' ' k 1 in
  let rec rline s k i arr =
    if i >= Array.length arr then
      cons k s
    else if arr.(i) = s then
      rline s (k + 1) (i + 1) arr
    else cons k s <|> rline (1 - s) 1 (i + 1) arr in
  ( xss |> List.rev |> List.map (rline 0 0 0) |> I.vcat
    |> I.vsnap ~align:`Top (h - 2) ) <->
  ( I.strf ~attr:A.(fg lightgreen ++ bg black) " RULE %d " rule
    |> I.vpad 1 0 )

let rule = 124 (* 110 mirrored *)

let main () =
  simpleterm_lwt_timed ~delay:0.1 ([], rule)
  ~f:(fun (w, h) (lines, rule) -> function
    | `Timer ->
        let prev  = match lines with [] -> [|1|] | h::_ -> h in
        let lines = step ~rule w prev :: lines |> take (h - 2) in
        `Redraw ((lines, rule), render ~rule ~h lines)
    | `Resize _ ->
        let lines = lines |> take h in
        `Redraw ((lines, rule), render ~rule ~h lines)
    | `Key (`Arrow `Left, []) ->
        `Redraw (([], rule - 1), render ~rule ~h lines)
    | `Key (`Arrow `Right, []) ->
        `Redraw (([], rule + 1), render ~rule ~h lines)
    | _ -> `Continue (lines, rule)
  )

let () = Lwt_main.run @@ main ()
