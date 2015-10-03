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
  "bold"     , bold
; "italic"   , italic
; "underline", underline
; "blink"    , blink
; "reverse"  , reverse
]

let image =
  let open List in
  let open I in
  let core16 =
    let c1  = map (fun (n, c) -> string A.(fg c) n) colors
    and c2  = map (fun (n, c) -> string A.(black @/ c @// empty) n) colors
    in vcat c1 <|> space 1 0 <|> vcat c2
  and rgb =
    let range = range 0 5 in
    range |> map (fun r ->
      range |> map (fun g ->
        range |> map (fun b ->
          char A.(bg (rgb ~r ~g ~b)) ' ' 1 1
        ) |> hcat
      ) |> vcat <|> space 1 0
    ) |> hcat
  and gray =
    range 0 23 |> map (fun level ->
      char A.(bg (gray ~level)) ' ' 1 1
    ) |> hcat
  and attr =
    styles |> map (fun (n, s) -> hpad 0 1 (string A.(s @+ empty) n)) |> hcat
  in
  intersperse (space 0 1) [core16; rgb; gray; attr]
  |> vcat |> pad ~left:1 ~top:1

let () = print image
