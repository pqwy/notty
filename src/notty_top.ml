(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

#if OCAML_VERSION >= (4,14,0)
let _ = Toploop.use_silently Format.err_formatter (Toploop.File "notty_top_init.ml")
#else
let _ = Toploop.use_silently Format.err_formatter "notty_top_init.ml"
#endif
