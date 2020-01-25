(* 1. *)
(* a) *)
let razlika_kvadratov x y = (x + y) * (x + y) - x * x - y * y

let a1 = razlika_kvadratov 5 3

(* b) *)
let uporabi_na_paru f (a, b) = (f a, f b)

let b1 = uporabi_na_paru (fun x -> x + 1) (3,4)

(* c) *)
let rec ponovi_seznam n list = 
  if n <= 0 then []
  else
  list @ (ponovi_seznam (n - 1) list)

let c1 = ponovi_seznam 3 [3;2;1]

(* d) *)
let razdeli list = 
  let rec razdeli' neg_acc acc = function
    | [] -> (neg_acc, acc)
    | x :: xs when x < 0 -> razdeli' (x :: neg_acc) acc xs
    | x :: xs -> razdeli' neg_acc (x :: acc) xs
  in
  razdeli' [] [] list

let d1 = razdeli [3;2;-5;1;-3]

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 2. *)
type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test_1 = Node (Node (leaf 3, 10, Node (leaf 14, 13, leaf 6)), 11, Node (leaf 2, 8, leaf 10))

let test_2 = Node (Node (leaf 20, 10, Node (leaf 15, 9, leaf 6)), 7, Node (leaf 1, 4, leaf 10))


let daljsi xs ys = if List.length xs >= List.length ys then xs else ys

let rec dol zgornja_meja = function  
  | Empty -> []
  | Node (l, x, r) when x > zgornja_meja -> [] 
  | Node (l, x, r) -> x :: (daljsi (dol x l) (dol x r)) 

let rec gor spodnja_meja = function  
  | Empty -> []
  | Node (l, x, r) when x < spodnja_meja -> [] 
  | Node (l, x, r) -> x :: (daljsi (gor x l) (gor x r)) 


let rec monotona_pot = function
  | Empty -> []
  | Node (l, x, r) ->
      let prvi_primer = (List.rev (dol x l)) @ [x] @ (gor x r) in
      let drugi_primer = (List.rev (gor x l)) @ [x] @ (dol x r) in
      let daljsa_tu = daljsi prvi_primer drugi_primer in
      let daljsa_poddrevesa = daljsi (monotona_pot l) (monotona_pot r) in
      daljsi daljsa_tu daljsa_poddrevesa

let a2_1 = monotona_pot test_1
let a2_2 = monotona_pot test_2








(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* 3. *)
type 'a veriga =
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

(* a) *)
let test = Filter ((>) 0, [], Filter ((>) 10, [], Ostalo []))

(* b) *)
let rec vstavi a = function
  | Ostalo xs -> Ostalo (a :: xs)
  | Filter (f, xs, veriga) when (f a) -> Filter (f, a :: xs, veriga)
  | Filter (f, xs, veriga) -> Filter (f, xs, vstavi a veriga)

let b3 = List.fold_left (fun fil x -> vstavi x fil) test [-5;7;100;-7;2]

(* c) *)
let posci a veriga = 
  let rec vsebuje = function
    | [] -> false
    | x :: xs when x = a -> true
    | x :: xs -> vsebuje xs
  in
  let rec posci' = function
    | Ostalo xs -> vsebuje xs
    | Filter (f, xs, chain) when (f a) -> vsebuje xs
    | Filter (f, xs, chain) -> posci' chain
  in
  posci' veriga
    
let c3_1 = posci 100 b3
let c3_2 = posci 101 b3

(* d) *)
let izprazni_filtre veriga =
  let rec stakni' acc = function
    | [] -> acc
    | x :: xs -> stakni' (x :: acc) xs
  in
  let stakni levi desni = stakni' desni (List.rev levi)
  in
  let rec zgradi' acc = function
    | [] -> acc
    | f :: fs -> zgradi' (Filter (f, [], acc)) fs
  in
  let zgradi f_acc = zgradi' (Ostalo []) f_acc
  in
  let rec izprazni_filtre' f_acc list_acc = function
    | Ostalo xs when (f_acc = []) -> (Ostalo [], xs) (* to se zgodi le če začetni samo Ostalo xs *)
    | Ostalo xs -> (zgradi f_acc, stakni list_acc xs)
    | Filter (f, xs, chain) -> izprazni_filtre' (f :: f_acc) (stakni list_acc (List.rev xs)) chain
  in
  izprazni_filtre' [] [] veriga

let rec izprazni = function
  | Ostalo xs -> (Ostalo [], xs)
  | Filter (f, xs, chain) ->
    let veriga, sez = izprazni chain in
    (Filter (f, [], veriga), xs @ sez)

let d3_1 = izprazni_filtre b3
let d3_2 = izprazni b3

(* e) *)
let dodaj_filter f veriga =
  let stara_veriga, seznam = (izprazni_filtre veriga)
  in
  let nova_veriga = Filter (f, [], stara_veriga)
  in
  List.fold_left (fun fil x -> vstavi x fil) nova_veriga seznam  

let e3 = dodaj_filter (fun x -> x mod 2 = 0) b3

