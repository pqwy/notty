open Notty
open Notty_unix

open Common

let () =
  Notty_unix.print_image_f @@ fun (w, h) ->
    Images.outline A.(fg lightblue)
      I.(hlimit (w - 2) @@
          vlimit (h - 3) @@ (* +1 for the prompt *)
            Images.sierp A.lightblue 5)
