(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty
open Notty.Infix

let pp_image_ansi = Render.pp Cap.ansi;;

#install_printer pp_image_ansi
