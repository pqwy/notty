open Notty
open Common

open Images

let input_proc_str str =
  let input = Unescape.create () in
  Unescape.input input str 0 (String.length str);
  let rec read () =
    match Unescape.next input with
    | `Await | `End -> () | _ -> read () in
  read ()

let escapes =
  let x = "\027[5~\027[6~\027[1~\027[4~\027OP\027OQ\027OR\027OS\027[15~\027[17~\027[18~\027[19~\027[20~\027[21~\027[23~\027[24~"
  in String.concat "" (List.replicate 960 x)

let chars = String.make 65280 'x'


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
    (fun () -> Operation.of_image (200, 200) i5)

let () =
  let measure = `Cputime in

  Unmark.time ~tag:"read escapes" ~measure ~n:1000
    (fun () -> input_proc_str escapes);
  Unmark.time ~tag:"read characters" ~measure ~n:1000
    (fun () -> input_proc_str chars)
