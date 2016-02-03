open Notty

let pow n e = int_of_float (float n ** float e)

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

module String = struct

  include String

  let repeat n str =
    let b = Buffer.create 16 in
    for i = 1 to n do Buffer.add_string b str done;
    Buffer.contents b
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
    tile 5 1 List.(
      range 0 15 |> map (fun i -> pad ~top:i ~left:(i*2) i2) |> zcat
    )


  let square color = I.string A.(fg color) "◾"

  let rec cantor = let open I in function
    | 0 -> square A.lightblue
    | n ->
        let sub = cantor (pred n) in
        hcat (List.replicate (pow 3 n) (square A.lightblue)) <->
        (sub <|> void (pow 3 (n - 1)) 0 <|> sub)

  let checker n m i =
    let w = I.width i in
    I.(tile (n/2) (m/2) (hpad 0 w i <-> hpad w 0 i))

  let checker1 = checker 20 20 I.(char A.(bg magenta) ' ' 2 1)

  let rec sierp c n = I.(
    if n > 1 then
      let ss = sierp c (pred n) in ss <-> (ss <|> ss)
    else hpad 1 0 (string A.(fg c) "◾")
  )

  let grid xxs = xxs |> List.map I.hcat |> I.vcat

  let outline attr i =
    let (w, h) = I.(width i, height i) in
    let chr x = I.uchar attr x 1 1
    and hbar  = I.uchar attr 0x2500 w 1
    and vbar  = I.uchar attr 0x2502 1 h in
    let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
    grid [ [a; hbar; b]; [vbar; i; vbar]; [d; hbar; c] ]
end

module Terminal = Notty_unix.Terminal

let simpleterm ~imgf ~f ~s =
  let term = Terminal.create () in
  let imgf (w, h) s =
    I.(string A.(fg darkgray) "[ESC quits.]" <-> imgf (w, h - 1) s) in
  let rec go s =
    Terminal.image term (imgf (Terminal.size term) s);
    match Terminal.input term with
    | `End | `Key (`Escape, []) -> ()
    | `Resize _                 -> go s
    | #Unescape.event as e      ->
        match f s e with Some s -> go s | _ -> ()
  in go s
