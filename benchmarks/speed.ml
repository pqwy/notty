(* Copyright (c) 2016-2019 David Kaloper Meršinjak. All rights reserved.
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


open Unmark

let strf = Format.asprintf
let group_of name xs f = group name (List.map f xs)
let bench_fmt fmt = Format.kasprintf bench fmt
let group_fmt fmt = Format.kasprintf group fmt

let render =

  let clip i = I.(width i |> min 200, height i |> min 200) in
  let ops i = Operation.of_image (0, 0) (clip i) i
  and render i = buf_render (0, 0) (clip i) i in

  group "render" [
    group "rasterize" [
      bench "i2" (fun () -> ops i2)
    ; bench "i3" (fun () -> ops i3)
    ; bench "i4" (fun () -> ops i4)
    ; bench "i5" (fun () -> ops i5)
    ];
    group "render" [
      bench "i2" (fun () -> render i2)
    ; bench "i3" (fun () -> render i3)
    ; bench "i4" (fun () -> render i4)
    ; bench "i5" (fun () -> render i5)
    ];
    group_f "draw" (fun t -> [
      bench "i3" (fun () -> Term.image t i3)
    ; bench "i5" (fun () -> Term.image t i5)
    ]) ~init:Term.create ~fini:Term.release
]

let input = group "input" [
  group "decode" [
    bench "escapes"     (fun () -> decode ~n:100 escapes);
    bench "CSI escapes" (fun () -> decode ~n:100 escapes_m);
    bench "chars"       (fun () -> decode ~n:100 chars);
  ];
  group "input" [
    bench "escapes"     (fun () -> input ~n:100 escapes);
    bench "CSI escapes" (fun () -> input ~n:100 escapes_m);
    bench "chars"       (fun () -> input ~n:100 chars);
  ]
]

let construct =

  let strings = [
      "s1", "a"
    ; "s2", "abcdefghij"
    ; "s3", String.repeat 10 "abcdefghij"
    ; "s4", String.repeat 100 "abcdefghij"
    ; "u1", "☭"
    ; "u2", String.repeat 10 "☭"
    ; "u3", String.repeat 100 "☭"
    ; "u4", String.repeat 1000 "☭" ] in

  group "construct" [

    group "make" (strings |> List.map @@ fun (n, s) ->
      bench n (fun () -> I.string A.empty s))

  ; group "repeat" ([0x40; 0x262d] |> List.map @@ fun x ->
      let u = Uchar.of_int x in
      group_fmt "U+%04x" x ([1; 10; 100] |> List.map @@ fun n ->
        bench_fmt "%dx" n (fun () -> I.uchar A.empty u n 1)))

  ; bench "pxmatrix" (fun () -> pxmatrix 200 200 @@ fun _ _ -> A.black)
  ]


let _ = Unmark_cli.main "Notty" [ render; input; construct ]
