(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * A few images that exercise image composition, cropping, and padding. This
 * test is a good canary.
 *)
open Common
open Notty_unix

let () = Images.[i3; i5; checker1] |> List.map eol |> List.iter output_image
