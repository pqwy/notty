open Notty
open Notty_unix

open Common

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let frame attr i =
  let (w, h) = I.(width i, height i) in
  let chr x = I.uchar attr (`Uchar x) 1 1
  and hbar  = I.uchar attr (`Uchar 0x2500) w 1
  and vbar  = I.uchar attr (`Uchar 0x2502) 1 h in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  grid [ [a; hbar; b]
       ; [vbar; i; vbar]
       ; [d; hbar; c] ]

let () =
  let (w, h) =
    match winsize Unix.stdout with Some d -> d | _ -> (0, 0) in
  let i =
    frame A.(fg lightblue)
      I.(hframe (w - 2) @@
          vframe (h - 3) @@ (* +1 for the prompt *)
            Images.sierp A.lightblue 5)
  in print_image i

