(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty

let pp_image_ansi = Render.pp Cap.ansi
let pp_color_ansi =
  let stencil = I.string "⏺" in
  fun ppf c -> pp_image_ansi ppf (I.attr (A.fg c) stencil)
let pp_attr_ansi =
  let stencil = I.string "attr" in
  fun ppf a -> pp_image_ansi ppf (I.attr a stencil)
;;

#install_printer pp_image_ansi
#install_printer pp_color_ansi
#install_printer pp_attr_ansi
