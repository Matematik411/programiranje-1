(* 1 *)

(* a) *)
let odstej_trojici (a, b, c) (d, e, f) = (a-d, b-e, c-f)

(* b) *)
let max_rezultat_do_n f n = 
  List.fold_left max (f n) (List.init n f)

let b1_1 = max_rezultat_do_n (fun x -> -x) 10

(* c) *)
let pocisti_seznam list = 
  let rec pocisti_seznam' acc = function
    | [] -> List.rev acc
    | (Some x) :: xs -> pocisti_seznam' (x :: acc) xs
    | _ :: xs -> pocisti_seznam' acc xs
  in
  pocisti_seznam' [] list

(* d) *)
let preveri_urejenost list = 
  let rec preveri_urejenenost' even_acc odd_acc = function
    | [] -> true
    | x :: xs when (x mod 2 = 0) -> (match even_acc with
                                    | None -> preveri_urejenenost' (Some x) odd_acc xs
                                    | Some y when x > y -> preveri_urejenenost' (Some x) odd_acc xs
                                    | Some y -> false)
    | x :: xs -> (match odd_acc with
                | None -> preveri_urejenenost' even_acc (Some x) xs
                | Some y when x < y -> preveri_urejenenost' even_acc (Some x) xs
                | Some y -> false)
  in preveri_urejenenost' None None list
    
let d1_1 = preveri_urejenost [5;2;4;1;6]
let d1_2 = preveri_urejenost [3;2;4;5;6]

(* 2. *)

(* a) *)
type 'a gnezdenje = 
  | Element of 'a 
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer = [
  Element 1;
  Element 2;
  Podseznam [
    Element 3;
    Podseznam [
      Element 4
    ];
    Podseznam []
  ];
  Podseznam [
    Element 5
  ]
]

(* b) *)
let rec najvecja_globina = function
  | [] -> 1
  | (Element _) :: xs -> najvecja_globina xs
  | (Podseznam x) :: xs -> max (1 + najvecja_globina x) (najvecja_globina xs)

let b2 = najvecja_globina gnezdenje_primer

(* c) *)
let rec preslikaj f = function
  | [] -> []
  | (Element a) :: xs -> (Element (f a)) :: (preslikaj f xs)
  | (Podseznam x) :: xs -> (Podseznam (preslikaj f x)) :: (preslikaj f xs)

let c2 = preslikaj (fun x -> x+1) gnezdenje_primer

(* d) *)
let rec splosci = function  
  | [] -> []
  | (Element a) :: xs -> a :: (splosci xs)
  | (Podseznam x) :: xs -> (splosci x) @ (splosci xs)

let d2 = splosci gnezdenje_primer

(* e) *)
let rec alternirajoci_konstruktorji = function
  | [] | _ :: [] -> true
  | a :: b :: xs -> (match (a, b) with
                    | (Element _, Podseznam _) | (Podseznam _, Element _) -> alternirajoci_konstruktorji (b :: xs)
                    | (_, _) -> false
                    )

let e2 = alternirajoci_konstruktorji gnezdenje_primer

(* f) *)

  (* | [] -> []
  | (Element a) :: xs -> (Element (f a)) :: (preslikaj f xs)
  | (Podseznam x) :: xs -> (Podseznam (preslikaj f x)) :: (preslikaj f xs) *)

let zlozi_preko_gnezdenja f zac list =
  let rec zlozi_preko_gnezdenja' f el = function
    | [] -> el
    | (Element a) :: xs -> zlozi_preko_gnezdenja' f (f el a) xs
    | (Podseznam x) :: xs -> zlozi_preko_gnezdenja' f (zlozi_preko_gnezdenja' f el x) xs

  in
  zlozi_preko_gnezdenja' f zac list


let zlozi_preko_gnezdenja_tlrec f zac list =
  let stakni levi desni = 
    let rec stakni' acc = function
    | [] -> acc
    | x :: xs -> stakni' (x :: acc) xs
  in stakni' (desni) (List.rev levi)

  in
  let rec zlozi_preko_gnezdenja' f el acc = function
    | [] when acc = [] -> el
    | (Element a) :: xs when acc = [] -> zlozi_preko_gnezdenja' f (f el a) acc xs
    | (Podseznam x) :: xs when acc = [] -> zlozi_preko_gnezdenja' f el (stakni x acc) xs
    | xs -> (
          match acc with
          | (Element b) :: ys -> zlozi_preko_gnezdenja' f (f el b) ys xs
          | (Podseznam y) :: ys -> zlozi_preko_gnezdenja' f el (stakni y ys) xs
          | _ -> failwith "error"
    )

  in
  zlozi_preko_gnezdenja' f zac [] list

let a = List.fold_left (fun x y -> x+y) 0 [1;2;3]
let f2 = zlozi_preko_gnezdenja (fun x y -> x+y) 0 gnezdenje_primer
let f2_tlrec = zlozi_preko_gnezdenja_tlrec (fun x y -> x+y) 0 gnezdenje_primer

(* 3. *)

(* funkcija naj bo memoizirana, zato jo pusitm *)
