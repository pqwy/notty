open Notty
open Common

let colors = A.[
  "black"        , black
; "red"          , red
; "green"        , green
; "yellow"       , yellow
; "blue"         , blue
; "magenta"      , magenta
; "cyan"         , cyan
; "lightgray"    , lightgray
; "darkgray"     , darkgray
; "lightred"     , lightred
; "lightgreen"   , lightgreen
; "lightyellow"  , lightyellow
; "lightblue"    , lightblue
; "lightmagenta" , lightmagenta
; "lightcyan"    , lightcyan
; "white"        , white
]

let styles = A.[
  "empty"      , empty
; "bold"       , st bold
; "italic"     , st italic
; "underline"  , st underline
; "blink"      , st blink
; "reverse"    , st reverse
; "bold/italic", bold @+ st italic
; "rev/underln", underline @+ st reverse
; "bold/rev"   , reverse @+ st bold
]

let image =
  let open List in
  let open I in
  let core16 =
    let c1  = map (fun (n, c) -> string A.(fg c) n) colors
    and c2  = map (fun (n, c) -> string A.(black @/ c @// empty) n) colors
    in vcat c1 <|> void 1 0 <|> vcat c2
  and rgb =
    let range = range 0 5 in
    range |> map (fun r ->
      range |> map (fun g ->
        range |> map (fun b ->
          char A.(bg (rgb ~r ~g ~b)) ' ' 1 1
        ) |> hcat
      ) |> vcat <|> void 1 0
    ) |> hcat
  and gray =
    range 0 23 |> map (fun level ->
      char A.(bg (gray ~level)) ' ' 1 1
    ) |> hcat
  and attr =
    styles |> map (fun (n, s) -> hpad 0 1 (string A.(red @/ s) n)) |> hcat
  in
  intersperse (void 0 1) [core16; rgb; gray; attr]
  |> vcat |> pad ~left:1 ~top:1

let () = Notty_unix.print_image_nl image
