## v0.2.2 (2019-02-19)

* Fix a long-standing terminal cleanup bug. Reported by @ttamttam, fix by @cfcs.

## v0.2.1 (2017-11-06)

* OCaml 4.06 compatible.
* Cache the internal representation of Unicode strings.
* Remove `I.ichar`. **breaking**

## v0.2.0 (2017-10-31)

* All-around speed and memory improvements.
* Draw over lines cell-by-cell instead of using erase-and-skip.
  Slower, but flicker-free drawing.
* `Term.create`: optionally inhibit synthetic TTY signals.
* Cursor origin moved from `(1, 1)` to `(0, 0)`. **breaking**
* `#key` renamed to `#special`. **breaking**
* Added `Term.fds` to get connected file descriptors.
* Added `A.equal` and `I.equal`.
* Switched over to `Uchar.t`. **breaking**
* Separated ASCII from the rest of Unicode input. **breaking**
* Added image pretty-printer `I.pp`.
* Added `notty.top` for use in the toplevel.
* Removed `I.tile`. **breaking**
* Added `I.tabulate`, generalizing `I.tile`.
* Added support for 24-bit color.
* Added `Notty_*.show_cursor` and `Notty_*.move_cursor` for manual cursor
  positioning in inline mode.
* Removed `output_image_endline`. Can be replaced by `eol`. **breaking**
* `Notty_*.output_image` lost the `~clear` parameter. Can be replaced in various
  ways by cursor positioning.
* `Notty_unix.output_image ~chan` renamed to `~fd`. **breaking**
* Added support for bracketed paste.
* More example programs.

## v0.1.1 (2016-02-09)
* `Term.input` -> `Term.event`
* Option to redraw the line

## v0.1.0 (2016-02-09)
* Initial release
