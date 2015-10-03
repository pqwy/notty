open Notty

module List = struct

  include List

  let rec replicate n a = if n < 1 then [] else a :: replicate (n - 1) a

  let rec range a b = if a > b then [] else a :: range (a + 1) b

  let rec intersperse a = function
    | [] | [_] as t -> t
    | x::xs         -> x :: a :: intersperse a xs

  let rec take n = function
    | x::xs when n > 0 -> x :: take (pred n) xs
    | _ -> []

  let rec splitat n = function
    | x::xs when n > 0 ->
        let (a, b) = splitat (pred n) xs in (x::a, b)
    | xs -> ([], xs)

  let rec chunks n xs =
    match splitat n xs with
    | (a, []) -> [a]
    | (a, b)  -> a :: chunks n b

  let rec zip xs ys = match (xs, ys) with
    | ([], _) | (_, []) -> []
    | (x::xs, y::ys) -> (x, y) :: zip xs ys

end

module Images = struct

  let i1 =
    let open I in
    (string A.(fg darkgray) "omgbbq" <->
      string A.(white @/ red @// empty) "@")
    <|> pad ~top:2 @@ string A.(green @/ empty) "xo"

  let i2 = I.(hpad 1 1 (hcrop 1 1 @@ tile 3 3 i1) <|> i1)

  let i3 = I.tile 5 5 i2

  let i4 =
    let i = I.(i3 <|> crop ~top:1 i3 <|> i3) in
    I.(crop ~left:1 i <-> crop ~right:1 i <-> crop ~bottom:2 i)

  let i5 =
    let open I in
    tile 5 1 @@
      List.(range 0 15 |> map (fun i -> pad ~top:i ~left:(i*2) i2)
                        |> zcat)


  let square color = I.string A.(fg color) "◾"

  let pow n e = int_of_float (float n ** float e)

  let rec cantor = let open I in function
    | 0 -> square A.lightblue
    | n ->
        let sub = cantor (pred n) in
        hcat (List.replicate (pow 3 n) (square A.lightblue)) <->
        (sub <|> space (pow 3 (n - 1)) 0 <|> sub)

  let checker n m c =
    let open I in
    let w = width c in
    let line i = List.replicate (m/2) i |> hcat in
    List.replicate (n/2)
      (line (hpad 0 w c) <-> line (hpad w 0 c))
    |> vcat

  let checker1 = checker 20 20 I.(char A.(bg magenta) ' ' 2 1)
end

let ch c = `Uchar (Char.code c)

let print i =
  print_image i; print_string "\n"; flush stdout

let simpleterm ~image ~step ~s =
  let term = Terminal.create () in
  let rec go s =
    match Terminal.input term with
    | `End -> ()
    | `Uchar _ | `Key _ as i ->
        match step s i with
        | Some s -> Terminal.update term (image s); go s
        | None   -> ()
  in
  Terminal.update term (image s);
  go s
