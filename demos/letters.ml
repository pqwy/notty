open Notty
open Common

let nw = 6
and nh = 5

let () =
  simpleterm ~s:[]
    ~f:(fun us -> function
      | `Key ((`Delete|`Backspace), _) ->
          Some (match us with _::xs   -> xs | _ -> us)
      | `Key (`Uchar u, _) -> Some (List.take (nw * nh) (u::us))
      | _  -> Some us)
    ~imgf:(fun _ us ->
      let open List in
      let uus = chunks nw (rev us) in
      mapi (fun i us ->
        mapi (fun j u ->
          I.uchar A.(white @/ rgb 0 i j @// empty) u 1 1
        ) us |> I.hcat
      ) uus |> I.vcat
      |> I.pad ~top:1 ~left:1
      |> I.hlimit ~align:`Left (nw + 1)
      |> I.tile nw 1)
