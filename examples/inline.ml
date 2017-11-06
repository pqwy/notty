(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Demonstrates manual cursor positioning. *)

open Notty
open Notty.Infix
open Notty_unix

let sleep n = flush stdout; Unix.select [] [] [] n |> ignore

let pp_str attr = I.pp_attr attr Format.pp_print_string

let rewind n = move_cursor `Home; move_cursor (`By (0, - (max n 0)))

let output_subst ~prev i =
  let h = I.height prev in
  let d = h - I.height i in
  if d > 0 then ( rewind (d - 1); output_image (I.void 0 d) );
  rewind (h - 1); output_image i

let cmyk = function
  | 0 -> A.rgb ~r:0 ~g:5 ~b:5
  | 1 -> A.rgb ~r:5 ~g:0 ~b:5
  | 2 -> A.rgb ~r:5 ~g:5 ~b:0
  | 3 -> A.rgb ~r:0 ~g:0 ~b:0
  | _ -> A.rgb ~r:5 ~g:5 ~b:5

let () =

  let (w, h) = match winsize Unix.stdout with
    Some dim -> dim | _ -> assert false
  and attr = A.(fg lightwhite ++ bg blue) in
  let img1 =
    I.(string attr "THE BLUE STRIPE ABOVE" <->
         tabulate 1 h (fun _ _ -> I.strf "HIDDEN"))
  and img2 =
    I.(strf "Top line. There's a %a above. ^^^"
         (pp_str attr) "blue stripe" |> vpad 0 2) in

  output_image img1; output_subst ~prev:img1 img2;

  output_image I.(string A.(fg white) "[..]" |> eol);
  for i = 0 to 5 do
    let a  = A.(bg (rgb ~r:i ~b:(5 - i) ~g:0)) in
    let bg = I.tabulate 1 i (fun _ -> I.strf "HIDDEN [%d]") |> eol
    and fg = I.char a ' ' 19 (5 - i) <|> I.char a '-' 1 (5 - i) |> eol in
    output_image bg; output_subst ~prev:bg fg;
  done;
  output_image I.(string A.(fg white) "[..]" |> vpad 0 2);

  let rec go prev n =
    if n <= w then
      let h = log (float n) |> truncate in
      let i = prev <|> I.tabulate 1 h (fun _ y -> I.char A.(bg (cmyk y)) ' ' 1 1) in
      output_subst ~prev i; sleep 0.01; go i (n + 1)
    else output_subst ~prev I.empty in
  show_cursor false;
  go I.empty 1;
  show_cursor true;

  output_image
    I.(strf "It doesn't say %a anywhere on screen, either."
        (pp_attr A.(fg white) Format.pp_print_string) "hidden" |> eol)

