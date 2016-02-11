# Notty

**Notty** is a declarative terminal library for OCaml structured around a notion
of composable images. It tries to abstract away the basic terminal programming
model, and provide one that is simpler and more expressive.

The core layout engine and IO codecs are pure platform-independent OCaml.
Distribution includes modules with input and output facilities for Unix, and Lwt
on Unix.

As an attempt to redefine terminal programming, **Notty** has to be
"opinionated". It assumes Unicode throughout, does not have universal support
for various terminals out there, and has a peculiar programming and rendering
model.

Check out the [documentation], [examples], or peek directly into the [interface]
file.

If you `./configure` with `--enable-examples`, you get several little demo
programs that also double as tests.

**Notty**'s core API was heavily influenced by Haskell's [Vty][vty].

[documentation]: https://pqwy.github.io/notty
[examples]: http://pqwy.github.io/notty/Notty.html#examples
[interface]: https://github.com/pqwy/notty/blob/master/src/notty.mli
[vty]: https://hackage.haskell.org/package/vty


![demo](https://raw.githubusercontent.com/pqwy/notty/blob/images/demo.gif)

```OCaml
(* Game of Life with ZX Spectrum kitsch. *)

let dot = I.uchar A.(fg lightred) 0x25cf 1 1

let background step (n, m) =
  let k = int_of_float @@ (sin (float (step + m + n) /. 10.)) *. 24. in
  if k > 0 then I.char A.(fg (gray k)) '.' 1 1 else I.void 1 1

let render (w, h) step life =
  0 -- (h - 2) |> List.map (fun m ->
    0 -- (w - 1) |> List.map (fun n ->
      let pt = (n, m) in if CSet.mem pt life then dot else background step pt
    ) |> I.hcat
  ) |> I.vcat
```

## Why?

**Q:**
**_Notty?_**

**A:**
Terminals are tedious to program for. Notty tries to abstract the tedium away,
leaving you with a more pleasant programming surface that's quite unlike a TTY.
Hence, **No-TTY**.

**Q:**
Why make yet another terminal library?

**A:**
Because:
  * It allows one to *describe* what should be seen, as opposed to *commanding*
    a terminal.
  * It's pretty compact. Both bells and whistles can be implemented separately.
  * Core is easy to glue onto various IO backends.
  * Pure platform-independent OCaml.
