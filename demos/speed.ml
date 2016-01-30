(* open Notty *)
(* open Notty_unix *)

(* open Common *)
(* open Common.Images *)

(* (1* let input_proc_str ?(n=1) str = *1) *)
(* (1*   let open Unescape in *1) *)
(* (1*   let rec drain i = *1) *)
(* (1*     match next_escape i with `Await|`End -> () | _ -> drain i in *1) *)
(* (1*   let i  = create () *1) *)
(* (1*   and sn = String.length str in *1) *)
(* (1*   for _ = 1 to n do input i str 0 sn; drain i done *1) *)

(* let escapes = *)
(*   "\027[5~\027[6~\027[1~\027[4~\027OP\027OQ\027OR\027OS\027[15~\027[17~\027[18~\027[19~\027[20~\027[21~\027[23~\027[24~" *)
(* let chars = String.(make (length escapes) 'x') *)

(* let () = Unmark.warmup () *)

(* (1* let () = *1) *)
(* (1*   let measure = `Cputime_ns in *1) *)

(* (1*   Terminal.(Unmark.time_ex ~tag:"draw i3" ~n:1000 *1) *)
(* (1*     ~measure ~init:create ~fini:release (fun t -> image t i3)); *1) *)
(* (1*   Terminal.(Unmark.time_ex ~tag:"draw i5" ~n:1000 *1) *)
(* (1*     ~measure ~init:create ~fini:release (fun t -> image t i5)); *1) *)

(* (1*   Unmark.time ~tag:"rasterize i2" ~measure ~n:300 *1) *)
(* (1*     (fun () -> Operation.of_image (200, 200) i2); *1) *)
(* (1*   Unmark.time ~tag:"rasterize i3" ~measure ~n:300 *1) *)
(* (1*     (fun () -> Operation.of_image (200, 200) i3); *1) *)
(* (1*   Unmark.time ~tag:"rasterize i4" ~measure ~n:300 *1) *)
(* (1*     (fun () -> Operation.of_image (200, 200) i4); *1) *)
(* (1*   Unmark.time ~tag:"rasterize i5" ~measure ~n:300 *1) *)
(* (1*     (fun () -> Operation.of_image (200, 200) i5); *1) *)

(* (1*   let ops3 = Operation.of_image (200, 200) i3 *)
(*   and ops5 = Operation.of_image (200, 200) i5 in *)
(*   Unmark.time ~tag:"eq i3/1" ~measure ~n:300 (fun () -> ops3 = ops3); *)
(*   Unmark.time ~tag:"eq i5/1" ~measure ~n:300 (fun () -> ops5 = ops5); *)
(*   Unmark.time ~tag:"eq i3/2" ~measure ~n:300 (fun () -> eqop ops3 ops3); *)
(*   Unmark.time ~tag:"eq i5/2" ~measure ~n:300 (fun () -> eqop ops5 ops5); *1) *)

(*   (1* () *1) *)

(* let () = *)
(*   let measure = `Cputime in *)

(*   (1* Unmark.time ~tag:"read escapes" ~measure ~n:100 *1) *)
(*   (1*   (fun () -> input_proc_str ~n:1000 escapes); *1) *)
(*   (1* Unmark.time ~tag:"read characters" ~measure ~n:100 *1) *)
(*   (1*   (fun () -> input_proc_str ~n:1000 chars) *1) *)

(* (1* let () = *1) *)
(* (1*   let measure = `Cputime_ns in *1) *)
(* (1*   let s1 = String.repeat 10 "☭" *1) *)
(* (1*   and s2 = String.repeat 1024 "☭" in *1) *)
(* (1*   Unmark.time ~tag:"construct small string" ~measure ~n:1000 *1) *)
(* (1*     (fun () -> I.string A.empty s1); *1) *)
(* (1*   Unmark.time ~tag:"construct big string" ~measure ~n:1000 *1) *)
(* (1*     (fun () -> I.string A.empty s2); *1) *)

(*   () *)

(* (1* let () = *1) *)
(* (1*   input_proc_str ~n:500000 escapes *1) *)
