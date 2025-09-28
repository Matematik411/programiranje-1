let pozdravi = function
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Filip" | "Žiga" -> "Oj!"
  | "" -> "Živjo!"
  | "*" -> "Dober dan!"
  | _ -> "Pozdravljen!"

let rec fakulteta = function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)

(* 0, 1, 1, 2, 3, 5, 8,  *)

let hitri_fib n =
  let rec aux n a b =
    if n = 0 then a else aux (n - 1) b (a + b)
  in aux n 0 1

let moj_nabor velikost =
  if velikost = 2 then (2, true) else (1, false)

let razdalja (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let rec dolzina xs =
  match xs with
  | [] -> 0
  | x :: xs' -> 1 + dolzina xs'

let rec vsota xs =
  match xs with
  | [] -> 0
  | x :: xs' -> x + vsota xs'

let rec skalarni_produkt xs ys =
  match (xs, ys) with
  | ([], []) -> 0.
  | (x :: xs', y :: ys') -> x *. y +. skalarni_produkt xs' ys'
  | _ -> failwith "Seznama morata biti enake dolžine"

let () =
  print_endline (pozdravi "Matija");
  print_endline (pozdravi "Filip");
  print_endline (pozdravi "");
  print_endline (pozdravi "*");
  Printf.printf "Fakulteta 5: %d\n" (fakulteta 5);
  Printf.printf "Hitri fib 7: %d\n" (hitri_fib 7);
  let (a, b) = moj_nabor 2 in Printf.printf "Moj nabor: (%d, %b)\n" a b;
  Printf.printf "Razdalja: %.2f\n" (razdalja (0.,0.) (3.,4.));
  Printf.printf "Dolzina: %d\n" (dolzina [1;2;3;4]);
  Printf.printf "Vsota: %d\n" (vsota [1;2;3;4]);
  Printf.printf "Skalarni produkt: %.2f\n" (skalarni_produkt [1.;2.;3.] [4.;5.;6.])