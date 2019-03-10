# Notty — Declaring terminals

<a href="https://asciinema.org/a/ZIXzn2ZmIxK39qoT3eJla5OyO" alt="dumper"><img src="https://asciinema.org/a/ZIXzn2ZmIxK39qoT3eJla5OyO.png" width="400"/></a>
<a href="https://asciinema.org/a/TsIhDJv5S00AB2biVmhHRzZ8I" alt="input"><img src="https://asciinema.org/a/TsIhDJv5S00AB2biVmhHRzZ8I.png" width="400"/></a>
<a href="https://asciinema.org/a/z1Pc0Mppg2JFzteZzdeigLwYc" alt="microdots"><img src="https://asciinema.org/a/z1Pc0Mppg2JFzteZzdeigLwYc.png" width="400"/></a>
<a href="https://asciinema.org/a/NgpF9Im8qfUICC39GDDAe9Ede" alt="rain"><img src="https://asciinema.org/a/R94gnHTQhCFJAsWpRfVlZWcUB.png" width="400"/></a>

Notty is a declarative terminal library for OCaml structured around a notion
of composable images. It tries to abstract away the basic terminal programming
model, providing something simpler and more expressive.

The core layout engine and IO codecs are pure platform-independent OCaml.
Distribution includes modules with input and output facilities for Unix, and Lwt
on Unix.

As an attempt to redefine terminal programming, Notty has to be
_opinionated_. It assumes Unicode throughout, does not have universal support
for various terminals out there, and has a peculiar programming and rendering
model.

Notty's core API was heavily influenced by Haskell's [Vty][vty].

## Where to start

Check out the [documentation], [examples], or peek directly into the [interface]
file.

Building with `dune build @examples` will produce several little example
programs that also double as tests.

```OCaml
(* Game of Life with ZX Spectrum kitsch. *)

let dot : image = I.uchar A.(fg lightred) (Uchar.of_int 0x25cf) 1 1

let background step (n, m) =
  let k = 24. *. sin (float (step + m + n) /. 10.) |> truncate in
  if k > 0 then I.char A.(fg (gray k)) '.' 1 1 else I.void 1 1

let render (w, h) step life : image =
  I.tabulate w (h - 1) @@ fun x y ->
    let pt = (x, y) in
    if CSet.mem pt life then dot else background step pt
```

[documentation]: https://pqwy.github.io/notty/doc
[examples]: http://pqwy.github.io/notty/doc/Notty.html#examples
[interface]: https://github.com/pqwy/notty/blob/master/src/notty.mli
[vty]: https://hackage.haskell.org/package/vty

## Why?

- _Notty?_
  
  Terminals are tedious to program for. Notty tries to abstract the tedium away,
  leaving you with a more pleasant programming surface that's quite unlike a TTY.
  Hence, _No-TTY_.
- Why make yet another terminal library?
  
  Because:
  * It allows one to *describe* what should be seen, as opposed to *commanding*
    a terminal.
  * It's pretty compact. Both bells and whistles can be implemented separately.
  * Core is easy to glue onto various IO backends.
  * Pure platform-independent OCaml.

[![Build Status](https://travis-ci.org/pqwy/notty.svg?branch=master)](https://travis-ci.org/pqwy/notty)
