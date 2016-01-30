open Notty
open Notty_unix

open Common
open Common.Images


let decode ?(n=1) str =
  let f cs _ = function `Uchar c -> c::cs | _ -> cs in
  let us = str |> Uutf.String.fold_utf_8 f [] |> List.rev in
  for _ = 1 to n do Unescape.decode us |> ignore done

let escapes =
  "\027[5~\027[6~\027[1~\027[4~\027OP\027OQ\027OR\027OS\027[15~\027[17~\027[18~\027[19~\027[20~\027[21~\027[23~\027[24~"

let escapes_m =
  "\027[<0;59;7M\027[<32;58;7M\027[<32;57;7M\027[<32;56;7M\027[<32;54;7M\027[<32;53;8M\027[<32;52;8M\027[<32;51;8M\027[<32;50;8M\027[<32;49;8M\027[<32;47;9M\027[<32;46;9M\027[<32;44;9M\027[<32;42;10M\027[<32;41;10M\027[<32;41;11M\027[<32;40;11M\027[<32;41;12M\027[<32;42;12M\027[<32;42;13M\027[<32;43;13M\027[<32;44;13M\027[<0;44;13m"

let chars = String.(make (length escapes) 'x')

let () = Unmark.warmup ()

let () =
  let measure = `Cputime_ns in

  Terminal.(Unmark.time_ex ~tag:"draw i3" ~n:1000
    ~measure ~init:create ~fini:release (fun t -> image t i3));
  Terminal.(Unmark.time_ex ~tag:"draw i5" ~n:1000
    ~measure ~init:create ~fini:release (fun t -> image t i5));

  Unmark.time ~tag:"rasterize i2" ~measure ~n:300
    (fun () -> Operation.of_image (200, 200) i2);
  Unmark.time ~tag:"rasterize i3" ~measure ~n:300
    (fun () -> Operation.of_image (200, 200) i3);
  Unmark.time ~tag:"rasterize i4" ~measure ~n:300
    (fun () -> Operation.of_image (200, 200) i4);
  Unmark.time ~tag:"rasterize i5" ~measure ~n:300
    (fun () -> Operation.of_image (200, 200) i5);

  (* let ops3 = Operation.of_image (200, 200) i3 *)
  (* and ops5 = Operation.of_image (200, 200) i5 in *)
  (* Unmark.time ~tag:"eq i3/1" ~measure ~n:300 (fun () -> ops3 = ops3); *)
  (* Unmark.time ~tag:"eq i5/1" ~measure ~n:300 (fun () -> ops5 = ops5); *)
  (* Unmark.time ~tag:"eq i3/2" ~measure ~n:300 (fun () -> eqop ops3 ops3); *)
  (* Unmark.time ~tag:"eq i5/2" ~measure ~n:300 (fun () -> eqop ops5 ops5); *)

  ()

let () =
  let measure = `Cputime in

  Unmark.time ~tag:"decode escapes" ~measure ~n:100
    (fun () -> decode ~n:1000 escapes);
  Unmark.time ~tag:"decode CSI escapes" ~measure ~n:100
    (fun () -> decode ~n:1000 escapes_m);
  Unmark.time ~tag:"decode chars" ~measure ~n:100
    (fun () -> decode ~n:1000 chars);
  (* Unmark.time ~tag:"read escapes" ~measure ~n:100 *)
  (*   (fun () -> input_proc_str ~n:1000 escapes); *)
  (* Unmark.time ~tag:"read characters" ~measure ~n:100 *)
  (*   (fun () -> input_proc_str ~n:1000 chars) *)

  ()

let () =
  let measure = `Cputime_ns in
  let s1 = String.repeat 10 "☭"
  and s2 = String.repeat 1024 "☭" in
  Unmark.time ~tag:"construct small string" ~measure ~n:1000
    (fun () -> I.string A.empty s1);
  Unmark.time ~tag:"construct big string" ~measure ~n:1000
    (fun () -> I.string A.empty s2);

  ()

(* let () = *)
(*   input_proc_str ~n:500000 escapes *)
