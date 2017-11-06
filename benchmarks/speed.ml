(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Notty
open Common
open Common.Images


let decode ?(n=1) str =
  let f cs _ = function `Uchar c -> c::cs | _ -> cs in
  let us = str |> Uutf.String.fold_utf_8 f [] |> List.rev in
  for _ = 1 to n do Unescape.decode us |> ignore done

let input ?(n=1) str =
  let buf = Bytes.unsafe_of_string str in
  let rec go f n = match Unescape.next f with
    | #Unescape.event -> go f n
    | `Await when n > 0 ->
        Unescape.input f buf 0 (Bytes.length buf); go f (pred n)
    | `Await -> ()
    | `End   -> assert false in
  go (Unescape.create ()) n

let escapes =
  "\027[5~\027[6~\027[1~\027[4~\027OP\027OQ\027OR\027OS\027[15~\027[17~" ^
  "\027[18~\027[19~\027[20~\027[21~\027[23~\027[24~"

let escapes_m =
  "\027[<0;59;7M\027[<32;58;7M\027[<32;57;7M\027[<32;56;7M\027[<32;54;7M" ^
  "\027[<32;53;8M\027[<32;52;8M\027[<32;51;8M\027[<32;50;8M\027[<32;49;8M" ^
  "\027[<32;47;9M\027[<32;46;9M\027[<32;44;9M\027[<32;42;10M\027[<32;41;10M" ^
  "\027[<32;41;11M\027[<32;40;11M\027[<32;41;12M\027[<32;42;12M" ^
  "\027[<32;42;13M\027[<32;43;13M\027[<32;44;13M\027[<0;44;13m"

let chars = String.(make (length escapes) 'x')

let buf = Buffer.create 1024
let buf_render off dim image =
  Buffer.clear buf; Render.to_buffer buf Cap.ansi off dim image

let b_render () =
  let measure = `Cputime_ns in

  let ops image () = Operation.of_image (0, 0) (200, 200) image in
  Unmark.time ~tag:"rasterize i2" ~measure ~n:1000 (ops i2);
  Unmark.time ~tag:"rasterize i3" ~measure ~n:1000 (ops i3);
  Unmark.time ~tag:"rasterize i4" ~measure ~n:1000 (ops i4);
  Unmark.time ~tag:"rasterize i5" ~measure ~n:1000 (ops i5);

  let render image () = buf_render (0, 0) (200, 200) image in
  Unmark.time ~tag:"render i2" ~measure ~n:1000 (render i2);
  Unmark.time ~tag:"render i3" ~measure ~n:1000 (render i3);
  Unmark.time ~tag:"render i4" ~measure ~n:1000 (render i4);
  Unmark.time ~tag:"render i5" ~measure ~n:1000 (render i5);

  Term.(Unmark.time_ex ~tag:"draw i3" ~n:1000
    ~measure ~init:create ~fini:release (fun t -> image t i3));
  Term.(Unmark.time_ex ~tag:"draw i5" ~n:1000
    ~measure ~init:create ~fini:release (fun t -> image t i5));

  (* let ops3 = Operation.of_image (200, 200) i3 *)
  (* and ops5 = Operation.of_image (200, 200) i5 in *)
  (* Unmark.time ~tag:"eq i3/1" ~measure ~n:300 (fun () -> ops3 = ops3); *)
  (* Unmark.time ~tag:"eq i5/1" ~measure ~n:300 (fun () -> ops5 = ops5); *)
  (* Unmark.time ~tag:"eq i3/2" ~measure ~n:300 (fun () -> eqop ops3 ops3); *)
  (* Unmark.time ~tag:"eq i5/2" ~measure ~n:300 (fun () -> eqop ops5 ops5); *)

  ()

let b_input () =
  let measure = `Cputime in

  Unmark.time ~tag:"decode escapes" ~measure ~n:100
    (fun () -> decode ~n:1000 escapes);
  Unmark.time ~tag:"decode CSI escapes" ~measure ~n:100
    (fun () -> decode ~n:1000 escapes_m);
  Unmark.time ~tag:"decode chars" ~measure ~n:100
    (fun () -> decode ~n:1000 chars);

  Unmark.time ~tag:"input escapes" ~measure ~n:100
    (fun () -> input ~n:1000 escapes);
  Unmark.time ~tag:"input CSI escapes" ~measure ~n:100
    (fun () -> input ~n:1000 escapes_m);
  Unmark.time ~tag:"input chars" ~measure ~n:100
    (fun () -> input ~n:1000 chars);

  ()

let b_construct () =
  let measure = `Cputime_ns in

  let strings = [
      "s1", "a"
    ; "s2", "abcdefghij"
    ; "s3", String.repeat 10 "abcdefghij"
    ; "s4", String.repeat 100 "abcdefghij"
    ; "u1", "☭"
    ; "u2", String.repeat 10 "☭"
    ; "u3", String.repeat 100 "☭"
    ; "u4", String.repeat 1000 "☭" ] in

  strings |> List.iter (fun (n, s) ->
    Unmark.time ~tag:("make " ^ n) ~measure ~n:1000
      (fun () -> I.string A.empty s));

  List.iter (fun x ->
    let u = Uchar.of_int x in
    for i = 0 to 2 do
      let n = 10. ** float i |> truncate in
      let tag = Format.sprintf "repeat U+%04X x %d" x n in
      Unmark.time ~tag ~measure ~n:1000
        (fun () -> I.uchar A.empty u n 1)
    done) [0x40; 0x262d];

  Unmark.time ~tag:"pxmatrix" ~measure:`Cputime ~n:100
    (fun () -> pxmatrix 200 200 @@ fun _ _ -> A.black);

  ()


let () =
  List.iter (fun f -> f ()) [
    Unmark.warmup
  ; b_render
  ; b_input
  ; b_construct
  ]
