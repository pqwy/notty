open Notty
open Common

let es = [
  [0x2e; 0x2e; 0x2e; 0x2e];
  [0x25aa; 0x25fe; 0x25fc; 0x2b1b];
  [0x1f346; 0x1f351; 0x1f605; 0x1f4a6];
  [0x1f62d; 0x1f52a; 0x1f52a; 0x1f47c];
]

let image =
  es |> List.(map (map @@ fun x ->
    let i = I.uchar A.(fg lightwhite) (Uchar.of_int x) 1 1 in
    I.(pad ~r:(3 - width i) i)
  )) |> Images.grid |> I.pad ~l:1 |> Images.outline A.(fg lightblack)

let () = Notty_unix.output_image_size @@ fun (w, _) ->
  I.(pad ~l:((w - width image) / 2) ~b:1 image)
