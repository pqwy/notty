open Notty
open Common

let () =
  simpleterm ~s:[]
    ~step:(fun us -> function
      | `Key ((`Delete|`Backspace), _) ->
          Some (match us with _::xs   -> xs | _ -> us)
      | `Key (`Uchar _ as u, _) -> Some (List.take 25 (u::us))
      | `Key (`Enter, _) -> None
      | _  -> Some us)
    ~image:(fun us ->
      let open List in
      let uus = chunks 5 (rev us) in
      mapi (fun i us ->
        mapi (fun j u ->
          I.uchar A.(white @/ rgb 0 i j @// empty) u 1 1
        ) us |> I.hcat
      ) uus |> I.vcat
      |> I.pad ~top:1 ~left:1
      |> I.hframe ~align:`Left 6
      |> I.tile 5 1)
