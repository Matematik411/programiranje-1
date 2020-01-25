(* 1. *)

(* a) *)
let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs -> let () = print_int x in izpisi_vsa_stevila xs

(* let a1 = izpisi_vsa_stevila [1;2;3;4] *)

(* b) *)
let map2_opt f list1 list2 =
  let rec map2_opt' acc = function
    | ([],[]) -> Some (List.rev acc)
    | ([], _) | (_,[]) -> None
    | (x :: xs, y :: ys) -> map2_opt' ((f x y) :: acc) (xs, ys)
  in
  map2_opt' [] (list1, list2)

let b1_1 = map2_opt (+) [1;2;3] [7;5;3]
let b1_2 = map2_opt (+) [1;2;3] [7;5]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 2. *)

(* a) *)
type filter_tree = 
  | Node of filter_tree * int * filter_tree
  | Box of int list

let primer = Node (Node (Box [1], 5, Box []), 10, Node (Box [], 15, Box [19;20]))


(* b) *)
let rec vstavi n = function
  | Box xs -> Box (n :: xs)
  | Node (l, a, r) ->
    if n <= a then Node (vstavi n l, a, r)
    else Node (l, a, vstavi n r)

let b2 = vstavi 12 primer

(* c) *)
let vstavi_seznam list tree = 
  let rec vstavi_seznam' acc = function
    | [] -> acc
    | x :: xs -> vstavi_seznam' (vstavi x acc) xs
  in
  vstavi_seznam' tree list

let c2 = vstavi_seznam [12;13;100] primer

(* d) *)
let rec preveri tree =
  let rec vsa_vecja lower = function
    | [] -> true
    | x :: xs when x >= lower -> vsa_vecja lower xs
    | x :: xs -> false 
  in
  let rec vsa_manjsa upper = function
    | [] -> true
    | x :: xs when x <= upper -> vsa_manjsa upper xs
    | x :: xs -> false 
  in
  let preveri_box lower upper list = 
    match (lower, upper) with
      | (None, None) -> true (* do tega pride le če na začetku samo en Box *)
      | (None, Some u) -> vsa_manjsa u list
      | (Some l, None) -> vsa_vecja l list
      | (Some l, Some u) -> vsa_vecja l list && vsa_manjsa u list
  in
  let rec skozi_drevo lower upper = function
    | Node (l, a, r) -> (skozi_drevo lower (Some a) l) && (skozi_drevo (Some a) upper r)
    | Box xs -> preveri_box lower upper xs
  in
  skozi_drevo None None tree

let d2_1 = preveri primer
let d2_2 = preveri (Node (Box [1], 5, Box [2;7]))

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 3. *)
type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  type t

  val id : t
  val uporabi : t -> vektor -> vektor
  val iz_matrike : matrika -> t
  val iz_funkcije : (vektor -> vektor) -> t
  val kompozitum : t -> t -> t
end

(* a) *)
module Matrika : Linearna = struct
  type t = matrika

  let id = (1,0,0,1)
  let uporabi (a, b, c, d) (v1, v2) = (a * v1 + b * v2, c * v1 + d * v2)
  let iz_matrike a = a
  let kompozitum (a1, b1, c1, d1) (a2, b2, c2, d2) = 
      (a1 * a2 + b1 * c2,
       a1 * b2 + b1 * d2,
       c1 * a2 + d1 * c2, 
       c1 * b2 + d1 * d2)
  let iz_funkcije f = 
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
end

(* b) *)
module Funkcija : Linearna = struct
  type t = vektor -> vektor

  let id = (fun x -> x)
  let uporabi f v = f v
  let iz_funkcije f = f
  let iz_matrike (a, b, c, d) = 
    (fun (x, y) -> (x * a + y * b, x * c + y * d))
  let kompozitum f g = (fun x -> f (g x))
end

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 4. *)
(* spet lažje preko memoizacije, saj poteze... *)




