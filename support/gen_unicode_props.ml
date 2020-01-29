#!/usr/bin/env ocaml
(* Copyright (c) 2020 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

#use "topfind"
#require "uucp"

let filter p seq i = seq (fun x -> if p x then i x)
let map f seq i = seq (fun x -> i (f x))
let uchars it =
  let rec go it u = it u; go it (Uchar.succ u) in
  try go it Uchar.min with Invalid_argument _ -> ()
let to_list seq =
  let xs = ref [] in
  seq (fun x -> xs := x :: !xs);
  List.rev !xs

let intervals_kv seq i =
  let s = ref None in
  let f (x, v) = match !s with
  | None -> s := Some (x, x, v)
  | Some (a, b, v0) when v = v0 && x = Uchar.succ b -> s := Some (a, x, v0)
  | Some e -> i e; s := Some (x, x, v) in
  seq f;
  match !s with Some e -> i e | _ -> ()

let intervals_p seq =
  map (fun x -> x, ()) seq |> intervals_kv |> map (fun (a, b, _) -> a, b)

(* Condenses code points into continuous range. *)
let pack_u u = let i = Uchar.to_int u in if i > 0xd7ff then i - 0x800 else i
let unpack_u i = Uchar.of_int (if i < 0xd800 then i else i + 0x800)

(* 8-8-8-bit trie, 3 levels, lowest level is string.

   Current packed Uchar.max is 0x10f7ff; trie can map up to 0xffffff distinct
   code points.

   Irregular (10-7-7 or 12-6-6) tries are marginally faster, but result in
   larger code footprint.

   First level is variable size, other two are either full or empty.
   *)
let trie ~default f =
  let xs = List.init ((pack_u Uchar.max lsr 16) + 1) @@ fun b0 ->
    let mask = b0 lsl 16 in
    let arr = Array.init 0x100 @@ fun b1 ->
      let mask = mask lor (b1 lsl 8) in
      let v b2 = match unpack_u (mask lor b2) with
      | x -> f x
      | exception Invalid_argument _ -> default in
      match (for b2 = 0 to 0xff do if v b2 <> default then raise Exit done) with
      | exception Exit -> String.init 0x100 (fun b2 -> Char.chr (v b2))
      | () -> ""
    in
    if Array.for_all ((=) "") arr then [||] else arr
  in
  let rec trim = function [||]::xs -> trim xs | xs -> xs in
  List.rev (trim (List.rev xs)) |> Array.of_list

let pf = Format.fprintf
let pp_iter ?(sep = fun _ _ -> ()) iter pp ppf x =
  let fst = ref true in
  let f x = (if !fst then fst := false else sep ppf ()); pp ppf x in
  iter f x
let pp_u ppf u = pf ppf "0x%04x" (Uchar.to_int u)
let pp_as_array iter pp ppf x =
  let sep ppf () = pf ppf ";@ " in
  pf ppf "@[<2>[|%a|]@]" (pp_iter ~sep iter pp) x

let dump_interval_map (ppf_mli, ppf_ml) ~name ~desc seq =
  pf ppf_mli "(* %s *)@.val %s: int array * int array * int array@.@." desc name;
  let xs = to_list (intervals_kv seq) in
  let aa = List.map (fun (a, _, _) -> a) xs
  and bb = List.map (fun (_, b, _) -> b) xs
  and cc = List.map (fun (_, _, c) -> c) xs in
  let pp_arr pp = pp_as_array List.iter pp in
  let pp_arr_u = pp_arr pp_u and pp_arr_i = pp_arr Format.pp_print_int in
  pf ppf_ml "@[<2>let %s =@ @[<1>(%a,@ %a,@ %a)@]@]@.@."
     name pp_arr_u aa pp_arr_u bb pp_arr_i cc

let dump_trie_map (ppf_mli, ppf_ml) ~name ~desc ~default f =
  pf ppf_mli "(* %s *)@.val %s: string array array@.@." desc name;
  let xs = trie ~default f in
  pf ppf_ml "let n = \"\"@."; (* "" is not unique, unlike [||] *)
  pf ppf_ml "@[<2>let %s =@ %a@]" name
    (pp_as_array Array.iter
      (pp_as_array Array.iter
        (fun ppf -> function "" -> pf ppf "n" | s -> pf ppf "%S" s)))
    xs

let pp_header ppf = Format.fprintf ppf
"(* Do not edit.
 *
 * This module contains select unicode properties extracted from Uucp,
 * using `%s`.
 *
 * Unicode version %s.
 *)

" Sys.argv.(0) Uucp.unicode_version

let extract (ppmli, ppml as ppfs) =

  pp_header ppmli; pp_header ppml;

  dump_interval_map ppfs
    ~name:"tty_width_hint"
    ~desc:"Uucp.Break.tty_width_hint"
    (* w = -1 is easy to detect.
       w = 1 covers the most intervals, so we default it. *)
    (uchars |> map (fun u -> u, Uucp.Break.tty_width_hint u)
            |> filter (fun (_, w) -> w <> -1 && w <> 1));

  (* dump_interval_map ppfs *)
  (*   ~name:"grapheme_cluster_boundary" *)
  (*   ~desc:"Uucp.Break.Low.grapheme_cluster." *)
  (*   (1* No single value dominates the histogram. *1) *)
  (*   (uchars |> map (fun u -> u, Uucp.Break.Low.grapheme_cluster u)); *)

  dump_trie_map ppfs
    ~name:"grapheme_cluster_boundary"
    ~desc:"Uucp.Break.Low.grapheme_cluster."
    ~default:16 (* 16 - `XX - is by far the most prevalent value *)
    Uucp.Break.Low.grapheme_cluster;

  ()

let file = "src/no-uucp/notty_uucp_data"

let with_new name f =
  let o = open_out_gen [Open_trunc; Open_creat; Open_wronly] 0o664 name in
  let ppf = Format.formatter_of_out_channel o in
  f ppf; Format.pp_print_flush ppf (); close_out o

let () =
  Format.printf "Dumping Unicode v%s data to %s.@." Uucp.unicode_version file;
  with_new (file ^ ".mli") @@ fun ppmli ->
    with_new (file ^ ".ml") @@ fun ppml ->
      extract (ppmli, ppml)
