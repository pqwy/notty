open Notty
open Notty_unix

open Common

let () =
  Notty_unix.output_image_size @@ fun (w, h) ->
    Images.outline A.(fg lightblue)
      I.(hsnap (w - 2) @@
          vsnap (h - 3) @@ (* +1 for the prompt *)
            Images.sierp A.lightblue 5)
