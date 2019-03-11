(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty
open Common

let () =
  Notty_unix.output_image_size @@ fun (w, h) ->
    Images.outline
      I.(hsnap (w - 2) @@
          vsnap (h - 3) @@ (* +1 for the prompt *)
            Images.sierp 5)
