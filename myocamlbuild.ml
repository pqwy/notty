open Ocamlbuild_plugin

(* XXX Generalize me somewhere? *)
let param n v = [A n; A v]
let params n vs = List.(vs |> map (param n) |> flatten)

let clib ?(tag="build_stubs") ?(use_tag="link_stubs") ~dir ~copts libname =

  (* https://github.com/ocaml/ocamlbuild/pull/94 *)
  flag ["c"; "compile"; tag] & S (params "-ccopt" copts);

  let libarg = "-l" ^ libname in
  (* `dllib` is not supported by _tags. *)
  flag ["link"; "ocaml"; "library"; "byte"; use_tag] &
    S (param "-dllib" libarg);
  (* https://github.com/ocaml/ocamlbuild/pull/94 *)
  flag ["link"; "ocaml"; "library"; "native"; use_tag] &
    S (param "-cclib" libarg);
  (* `include` tag is not active during linking. OCB bug? *)
  flag ["link"; "ocaml"; "program"] & S (param "-I" dir);

  let libn = [dir^"/lib"^libname^"."^ !Options.ext_lib] in
  dep ["link"; "ocaml"; use_tag] libn;
  dep ["compile"; "ocaml"; use_tag] libn

let () = dispatch @@ function
  | After_rules ->
      (* Enable `use_LIB` tags for linking against archives. *)
      List.iter ocaml_lib ["src/notty"; "unix/notty_unix"; "lwt/notty_lwt"];
      clib ~tag:"build_stubs"
           ~use_tag:"link_stubs"
           ~dir:"unix"
           ~copts:["-O3"; "-Wall"; "-Wextra"; "-Werror"]
           "notty_unix_stubs"
  | _ -> ()
