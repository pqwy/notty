#!/usr/bin/env ocaml
(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* This is meant to be run with uucp.1.1.0, pre-Uchar. *)

#use "topfind"
#require "unix"
#require "sequence"
#require "uucp"

let uchars it =
  for u = 0 to 0xd7ff do it u done;
  for u = 0xe000 to 0x10ffff do it u done

let ranges ~succ xs =
  let rec go (a, b as e) = function
    | [] -> [e]
    | x::xs when x = succ b -> go (a, x) xs
    | x::xs -> e :: go (x, x) xs in
  match xs with x::xs -> go (x, x) xs | _ -> []

let uchars_with_p p =
  Sequence.(uchars |> filter p |> to_list) |> ranges ~succ:succ

let pp_dump_ranges (ppmli, ppml) ~name ~desc xs =
  let pp_u ppf = Format.fprintf ppf "0x%04X" in
  Format.fprintf ppmli "(* %s *)\nval %s : (int * int) array\n" desc name;
  Format.fprintf ppml "let %s = [|\n%a|]\n\n" name
    (fun ppf -> List.iter @@ fun (a, b) ->
      Format.fprintf ppf "  (%a, %a);\n" pp_u a pp_u b)
    xs

let pp_uchars_with_p ppfs ~name ~desc p =
  pp_dump_ranges ppfs ~name ~desc (uchars_with_p p)
let pp_date ppf t = let open Unix in
  Format.fprintf ppf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let pp_header ppf = Format.fprintf ppf
"(* Do not edit.
 *
 * This module contains Unicode scalar value ranges with particular properties,
 * extracted from Uucp using `%s`.
 *
 * Using Unicode %s, on %a.
 *)

" Sys.argv.(0) Uucp.unicode_version pp_date Unix.(gmtime (gettimeofday ()))

let extract (ppmli, ppml as ppfs) =

  pp_header ppmli; pp_header ppml;

  pp_uchars_with_p ppfs
    ~name:"eaw__w_f" ~desc:"East Asian Width in {W, F}"
    (fun u -> let w = Uucp.Break.east_asian_width u in w = `W || w = `F);

  pp_uchars_with_p ppfs
    ~name:"gc__mn_me_cf" ~desc:"General Category in {Mn, Me, Cf}"
    (fun u -> let c = Uucp.Gc.general_category u in c = `Mn || c = `Me || c = `Cf)


let file = "src/notty_unicode_data"

let with_new name f =
  let o = open_out_gen [Open_trunc; Open_creat; Open_wronly] 0o664 name in
  let ppf = Format.formatter_of_out_channel o in
  f ppf; Format.pp_print_flush ppf (); close_out o

let () =
  with_new (file ^ ".mli") @@ fun ppmli ->
    with_new (file ^ ".ml") @@ fun ppml ->
      extract (ppmli, ppml)
