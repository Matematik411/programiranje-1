(* naloga 31 Euler *)
let available = [|200;100;50;20;10;5;2;1|]

let count value coins =
  let rec how_many a here =
    if here = (Array.length coins) - 1 then 1
    else
    if coins.(here) > a then how_many a (here + 1)
    else (how_many (a - coins.(here)) here) + (how_many a (here + 1))
  in
  how_many value 0

let () = print_int (count 200 available)