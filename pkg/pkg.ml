#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"
open Topkg

let unix  = Conf.with_pkg ~default:true "unix"
and lwt   = Conf.with_pkg ~default:false "lwt"
and speed = Conf.(key ~doc:"Build benchmarks" "benchmarks" bool ~absent:false)
and ex    = Conf.(key ~doc:"Build examples" "examples" bool ~absent:false)

let bin ?cond name = Pkg.test ~run:false ?cond name
  (* Pkg.bin ?cond ~dst:Fpath.("bin" // "notty." ^ basename name) name *)

let build = Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let () =
  Pkg.describe "notty" ~build @@ fun c ->
    let unix = Conf.value c unix in
    let lwt = unix && Conf.value c lwt
    and speed = unix && Conf.value c speed
    and ex0 = unix && Conf.value c ex in
    let ex1 = ex0 && lwt in
    Ok [ Pkg.mllib "src/notty.mllib";
         Pkg.mllib ~api:[] "src/notty_top.mllib";
         Pkg.lib "src/notty_top_init.ml";
         Pkg.mllib ~cond:unix "unix/notty_unix.mllib";
         Pkg.mllib ~cond:lwt "lwt/notty_lwt.mllib";
         Pkg.clib ~cond:unix "unix/libnotty_unix_stubs.clib";

         bin ~cond:speed "benchmarks/speed";

         bin ~cond:ex0 "examples/testpatterns";
         bin ~cond:ex0 "examples/letters";
         bin ~cond:ex0 "examples/keys";
         bin ~cond:ex0 "examples/colors";
         bin ~cond:ex0 "examples/sierpinski";
         bin ~cond:ex0 "examples/cuts";
         bin ~cond:ex0 "examples/thisbig";
         bin ~cond:ex0 "examples/runes";
         bin ~cond:ex0 "examples/crops";
         bin ~cond:ex0 "examples/mouse";
         bin ~cond:ex0 "examples/cursor";
         bin ~cond:ex1 "examples/sierpinski_lwt";
         bin ~cond:ex1 "examples/life";
         bin ~cond:ex1 "examples/linear";
         bin ~cond:ex1 "examples/emoji";
    ]
