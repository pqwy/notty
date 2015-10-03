open Notty
open Common

let () =
  let open I in
  let s   = "x◾e\204\129◾x" in
  let sp  = string A.(bg darkgray) " "
  and att = A.(lightmagenta @/ darkgray @// empty) in
  print
    List.(range 0 5 |> map (fun i ->
      range 0 (5 - i) |> map (fun j ->
        sp <|> crop ~left:i ~right:j (string att s) <|> sp
        ) |> vcat |> pad ~right:2
      ) |> hcat)
