(* 1. *)

(* a) *)
let podvoji_vsoto a b = 2 * (a + b)

let a1 = podvoji_vsoto 2 3

(* b) *)
let povsod_vecji (a, b, c) (d, e, f) = a > d && b > e && c > f

let b1_1 = povsod_vecji (3, 3, 3) (2, 2, 2)
let b1_2 = povsod_vecji (3, 3, 3) (2, 3, 2)


(* c) *)
let uporabi_ce_lahko f = function
  | None -> None
  | Some a -> Some (f a)

let c1_1 = uporabi_ce_lahko (fun x -> 3*x) None
let c1_2 = uporabi_ce_lahko (fun x -> 3*x) (Some 3)

(* d) *)
let pojavi_dvakrat a list =
  let rec pojavi_dvakrat' a acc = function
    | [] -> false
    | x :: xs when x = a -> if acc = 1 then true else pojavi_dvakrat' a 1 xs
    | x :: xs -> pojavi_dvakrat' a acc xs
  in
  pojavi_dvakrat' a 0 list

let d1 = pojavi_dvakrat 3 [1;2;33;4;3]

(* e) *)
let izracunaj_v_tocki a seznam_funkcij =
  let rec izracunaj_v_tocki' a acc = function
    | [] -> List.rev acc
    | f :: xs -> izracunaj_v_tocki' a ((f a) :: acc) xs
  in
  izracunaj_v_tocki' a [] seznam_funkcij

let e1 = izracunaj_v_tocki 2 [(fun x -> 3*x);(fun x -> 2*x)]

(* f) *)
let eksponent a n =
  if n < 0 then failwith "računamo eksponent celih števil"
  else
  let rec eksponent' a acc = function
    | 0 -> acc
    | n -> eksponent' a (a  * acc) (n - 1)
  in
  eksponent' a 1 n

let f1_1 = eksponent 1 1000000
let f1_2 = eksponent 2 3


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 2. *)

(* a) *)
type 'a mm_drevo =
  | Empty
  | Node of 'a mm_drevo * 'a * int * 'a mm_drevo

let testno = Node (Node (Empty, 1, 3, Empty), 2, 2, Node (Node (Empty, 4, 1, Empty), 5, 1, Node (Empty, 8, 2, Empty)))

(* b) *)
let rec vstavi drevo a = 
  match drevo with
  | Empty -> Node (Empty, a, 1, Empty)
  | Node (l, x, n, r) ->
    if x = a then Node (l, x, n+1, r)
    else
    if a < x then Node (vstavi l a, x, n, r)
    else
    Node (l, x, n, vstavi r a)

let b2 = vstavi testno 3

(* c) *)
let multimnozica_iz_seznama list = 
  let rec multimnozica_iz_seznama' acc = function
    | [] -> acc
    | x :: xs -> multimnozica_iz_seznama' (vstavi acc x) xs
  in
  multimnozica_iz_seznama' Empty list

let c2 = multimnozica_iz_seznama [3;2;5;3;3;3]

(* d) *)
let rec velikost_multimnozice = function
  | Empty -> 0
  | Node (l, _, n, r) -> n + (velikost_multimnozice l) + (velikost_multimnozice r)

let d2 = velikost_multimnozice c2


(* e) *)
(* CONTINUATION PASSING STYLE *)
let veckrat x n list =
  let rec aux x acc = function
    | 0 -> acc
    | m -> aux x (x :: acc) (m-1)
  in
  aux x list n

let seznam_iz_multimnozice drevo =
  let rec seznam_iz_multimnozice' acc k = function
  | Empty -> k acc
  | Node (l, x, n, r) -> 
  let k t_l = seznam_iz_multimnozice' (veckrat x n t_l) k l
  in
  seznam_iz_multimnozice' acc k r

  in seznam_iz_multimnozice' [] (fun x -> x) drevo


(* Z NOVIM TIPOM *)
type 'a naloga = 
  | Obdelaj of 'a mm_drevo
  | Element of 'a * int

let seznam_iz_mm drevo = 
  let rec seznam_iz_mm' gradimo = function  (* gledamo vrsto (seznam) 'a nalog *) 
    | [] -> gradimo (* smo konec *) 
    | (Element (x, n)) :: xs -> seznam_iz_mm' (veckrat x n gradimo) xs
    | (Obdelaj Empty) :: xs -> seznam_iz_mm' gradimo xs
    | (Obdelaj Node (l, x, n, r)) :: xs -> 
      seznam_iz_mm' gradimo (Obdelaj r :: Element (x, n) :: Obdelaj l :: xs)
  in
  seznam_iz_mm' [] [Obdelaj drevo]



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 3. *)






