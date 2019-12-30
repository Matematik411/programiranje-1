(* 1. *)

(* a) *)
let uporabi f a = f a

(* b) *)
let ibaropu a f = f a

(* c) *)
let zacetnih n list = 
  let rec zacetnih' acc = function
    | (0, _) -> Some (List.rev acc)
    | (_, []) -> None
    | (n, x :: xs) -> zacetnih' (x :: acc) (n - 1, xs)
  in
  zacetnih' [] (n, list)

let c1_1 = zacetnih 3 [1;2;3;4;5]
let c1_2 = zacetnih 6 [1;2;3;4;5]


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 2. *)
type 'a neprazen_sez =
  | Konec of 'a 
  | Sestavljen of 'a * 'a neprazen_sez

let testni = Sestavljen (1, Sestavljen (2, Sestavljen (3, Konec 4)))

(* a) *)
let rec prvi = function
  | Konec a -> a (* do tega pride le, če samo en element *)
  | Sestavljen (a, xs) -> a

let rec zadnji = function
  | Konec a -> a
  | Sestavljen (_, xs) -> zadnji xs

let a2_1 = prvi testni
let a2_2 = zadnji testni

(* b) *)
let rec dolzina = function
  | Konec _ -> 1
  | Sestavljen (_, xs) -> 1 + dolzina xs

let b2 = dolzina testni

(* c) *)
let pretvori_v_seznam neprazen = 
  let rec pretvori_v_seznam' acc = function
    | Konec a -> List.rev (a :: acc)
    | Sestavljen (a, xs) -> pretvori_v_seznam' (a :: acc) xs
  in
  pretvori_v_seznam' [] neprazen

let c2 = pretvori_v_seznam testni

(* d) *)
let rec zlozi f el = function
  | Konec a -> f el a
  | Sestavljen (a, xs) -> zlozi f (f el a) xs

let d2 = zlozi (fun x y -> x + y) 0 testni

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 3. *)











