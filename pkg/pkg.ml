#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let unix  = Conf.with_pkg ~default:true "unix"
and lwt   = Conf.with_pkg ~default:false "lwt"
and speed = Conf.(key ~doc:"Build benchmarks" "benchmarks" bool ~absent:false)
and demos = Conf.(key ~doc:"Build examples" "examples" bool ~absent:false)

let bin ?cond name = Pkg.test ~run:false ?cond name
  (* Pkg.bin ?cond ~dst:Fpath.("bin" // "notty." ^ basename name) name *)

let () =
  Pkg.describe "notty" @@ fun c ->
    let unix = Conf.value c unix in
    let lwt = unix && Conf.value c lwt
    and speed = unix && Conf.value c speed
    and demos = unix && Conf.value c demos in
    let demos' = demos && lwt in
    Ok [ Pkg.mllib "src/notty.mllib";
         Pkg.mllib ~cond:unix "unix/notty_unix.mllib";
         Pkg.mllib ~cond:lwt "lwt/notty_lwt.mllib";
         Pkg.clib ~cond:unix "unix/libnotty_unix_stubs.clib";

         bin ~cond:speed "benchmarks/speed";

         bin ~cond:demos "examples/testpatterns";
         bin ~cond:demos "examples/letters";
         bin ~cond:demos "examples/keys";
         bin ~cond:demos "examples/colors";
         bin ~cond:demos "examples/sierpinski";
         bin ~cond:demos "examples/cuts";
         bin ~cond:demos "examples/thisbig";
         bin ~cond:demos "examples/runes";
         bin ~cond:demos "examples/crops";
         bin ~cond:demos "examples/mouse";
         bin ~cond:demos "examples/cursor";
         bin ~cond:demos' "examples/sierpinski_lwt";
         bin ~cond:demos' "examples/life";
         bin ~cond:demos' "examples/linear";
    ]
