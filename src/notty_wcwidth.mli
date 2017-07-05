(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** {1 The guessing game.} *)

val wcwidth : Uchar.t -> int
(** [wcwidth u] guesses the character cell width that a terminal will assign to [u].

    {b Note} This is not necessarily the number of cells that a character is {i
    rendered as}; rather, it's the number of cells that rendering [u] will shift
    the cursor by. These two can, and often do, disagree.

    This function is morally just {!Uucp.Break.tty_width_hint}.

    Apart from simple fixed heuristics, it depends on the Unicode properties
    {{: http://www.unicode.org/reports/tr44/#General_Category_Values} General
    Category} and {{: http://www.unicode.org/reports/tr11} East Asian Width}.
    Unicode 9.0.0 {{: http://www.unicode.org/reports/tr11/tr11-30.html}updates}
    the latter, and many {i emojis} are now considered wide. However, most
    terminals in the wild have the equivalent of their own private copy of
    obsolete Unicode data. Since [Uucp] closely tracks the Unicode standard,
    [tty_width_hint] no longer agrees with them. For this reason, it is
    replicated here as [wcwidth], using baked-in Unicode 8.0.0 data.

    At the time of writing, some highlights are:
    {ul
    {- [rxvt-unicode] {{:
       https://github.com/exg/rxvt-unicode/blob/25b6c7424c232ea83ef4e3d79927223037bede0e/src/rxvt.h#L645}uses}
       the system [wcwidth].}
    {- [xterm] contains a copy of Markus Kuhn's
       {{: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c}portable [wcwidth]},
       locally extended with Unicode 6.2.0 and indeterminate bits of 9.0.0.}
    {- [iTerm2] {{:
       https://github.com/gnachman/iTerm2/blob/896069ed3b8d8f5fd51e086f579e3babbb0cdb62/sources/NSCharacterSet%2BiTerm.h}
       has its own} database, apparently containing Unicode 6.0 data.}
    {- [glibc] includes Unicode 8.0 as of {{:
       https://sourceware.org/git/?p=glibc.git;a=commit;h=23256f5ed889266223380c02b2750d19e3fe666b}
       [23256f5ed889]}, but {{:
       https://sourceware.org/git/?p=glibc.git;a=commit;h=925fac7793ba912172810767ac284a43045d41d2}
       [925fac7793ba]} - to be released in [glibc-2.26] - adds 10.0.0.
       (See [localedata/unicode-gen/].)}
    {- [neovim] {{:
       https://github.com/neovim/neovim/blob/008b604bacbbeeaf0e04f94b1d331b11ebec631a/src/nvim/mbyte.c#L432}uses}
       the system [wcwidth], but updates the result using a private copy of
       UTS-51 emoji data.}
    {- [vte] {{:
       https://github.com/GNOME/vte/blob/a26a60b8e564968b61716f0d2e52e17cd9362413/src/vte.cc#L116}
       broadly replicates} [wcwidth], using [glib]'s data which is at 10.0.0.
       (See [glib/gen-unicode-tables.pl] and the files it generates.)}}

    This function is basically everything that's wrong with everything, and
    should be removed as soon as Unicode 9.0.0+ gets significant uptake. This
    means that terminals draw e.g. [U+25FE] ('◾') as double-wide. *)
