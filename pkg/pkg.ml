#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"
open Topkg

let unix  = Conf.with_pkg ~default:true "unix"
and lwt   = Conf.with_pkg ~default:false "lwt"
and speed = Conf.(key ~doc:"Build benchmarks" "benchmarks" bool ~absent:false)
and demos = Conf.(key ~doc:"Build examples" "examples" bool ~absent:false)

let bin ?cond name = Pkg.test ~run:false ?cond name
  (* Pkg.bin ?cond ~dst:Fpath.("bin" // "notty." ^ basename name) name *)

let build = Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let () =
  Pkg.describe "notty" ~build @@ fun c ->
    let unix = Conf.value c unix in
    let lwt = unix && Conf.value c lwt
    and speed = unix && Conf.value c speed
    and demos0 = unix && Conf.value c demos in
    let demos1 = demos0 && lwt in
    Ok [ Pkg.mllib "src/notty.mllib";
         Pkg.mllib ~cond:unix "unix/notty_unix.mllib";
         Pkg.mllib ~cond:lwt "lwt/notty_lwt.mllib";
         Pkg.clib ~cond:unix "unix/libnotty_unix_stubs.clib";

         bin ~cond:speed "benchmarks/speed";

         bin ~cond:demos0 "examples/testpatterns";
         bin ~cond:demos0 "examples/letters";
         bin ~cond:demos0 "examples/keys";
         bin ~cond:demos0 "examples/colors";
         bin ~cond:demos0 "examples/sierpinski";
         bin ~cond:demos0 "examples/cuts";
         bin ~cond:demos0 "examples/thisbig";
         bin ~cond:demos0 "examples/runes";
         bin ~cond:demos0 "examples/crops";
         bin ~cond:demos0 "examples/mouse";
         bin ~cond:demos0 "examples/cursor";
         bin ~cond:demos1 "examples/sierpinski_lwt";
         bin ~cond:demos1 "examples/life";
         bin ~cond:demos1 "examples/linear";
    ]
