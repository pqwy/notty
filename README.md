# Notty

**Notty** is a declarative terminal graphics library for OCaml, structured
around a notion of composable images.

Check out the [documentation], [examples], or peek directly into the
[interface].

**Notty**'s core API was heavily influenced by Haskell's [Vty][vty].

**Please note that Notty is still unreleased and the interface is subject to
change.**

[documentation]: https://pqwy.github.io/notty
[examples]: http://pqwy.github.io/notty/Notty.html#1_Examples
[interface]: https://github.com/pqwy/notty/blob/master/src/notty.mli
[vty]: https://hackage.haskell.org/package/vty

## Why?

**Q:**
Why make it when we alredy have both **Foo** and **Meh2000**?

**A:**
Because:
  * It allows one to *describe* what should be seen, as opposed to *commanding*
    a terminal.
  * It is pretty compact. Both bells and whistles alike can be implemented
    separately.
  * Its core is easy to glue onto various IO frameworks.

**Q:**
"*Notty*?"

**A:**
Terminals are tedious to program for. *Notty* tries to abstract the tedium away,
leaving you with a more pleasant programming surface that's quite unlike a TTY.
Hence, **No-TTY**.
