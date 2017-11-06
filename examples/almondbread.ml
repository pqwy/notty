open Notty
open Common

let iter = 200

let member x y =
  let rec go cx cy x y n =
    let xx = x *. x and yy = y *. y in
    if n = 0 || xx +. yy > 4. then n else
      go cx cy (xx -. yy +. cx) (2. *. x *. y +. cy) (n - 1) in
  float (iter - go x y 0. 0. iter) /. float iter

let pi2   = 2. *. 3.14159
let pi2_3 = pi2 /. 3.

let mandelbrot x y =
  (* let esc = 1. -. member x y in *)
  (* 23. *. esc *. esc |> truncate |> A.gray *)
  match member x y with
  | 1.  -> A.gray 0
  | esc ->
      let t = esc *. pi2 in
      let f d = (sin (t +. d) *. 128. +. 128.) |> truncate in
      A.rgb_888 ~b:(f (-.pi2_3)) ~g:(f 0.) ~r:(f pi2_3)

let xlate dx dy f x y = f (x -. dx) (y -. dy)
let scale k f = let k1 = 1./.k in fun x y -> f (x *. k1) (y *. k1)
let rot a f =
  let sina = sin a and cosa = cos a in fun x y ->
    f (x *. cosa -. y *. sina) (x *. sina +. cosa *. y)

let render_unit f (w, h) =
  let sw = 1. /. float w
  and sh = 1. /. float (2 * h) in
  pxmatrix w h (fun x y -> f (float x *. sw) (float y *. sh))

let () =
  let pix =
    render_unit @@
    rot (-1.570795) @@ xlate (1.6) (-0.5) @@
    mandelbrot in
  Notty_unix.(output_image_size @@ fun (w, h) -> pix (w, h - 1) |> eol)
