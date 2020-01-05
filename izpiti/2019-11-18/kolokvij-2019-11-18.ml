(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root a b = a * a = b && a > 0

let pack3 a b c = (a, b, c) 

let sum_if_not pogoj = 
  let rec sum_if_not' acc = function
    | [] -> acc
    | x :: xs when (pogoj x) -> sum_if_not' acc xs
    | x :: xs -> sum_if_not' (x + acc) xs
  in
  sum_if_not' 0

let apply funkcije elementi = 
  let rec apply' a acc = function
    | [] -> List.rev acc
    | f :: fs -> apply' a (f a :: acc) fs
  in
  let podseznam a = apply' a [] funkcije
  in
  let rec vrni celota = function
    | [] -> List.rev celota
    | x :: xs -> vrni ((podseznam x) :: celota) xs
  in vrni [] elementi

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = 
  | Predavanje
  | Vaje

type srecanje = {predmet : string; vrsta : vrsta_srecanja; trajanje : int}

type urnik = srecanje list list

let vaje = {predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 3}
let predavanje = {predmet = "Programiranje 1"; vrsta = Predavanje; trajanje = 2}

let urnik_profesor = [
  [{predmet = "pon_vaje_1"; vrsta = Vaje; trajanje = 1}; {predmet = "pon_vaje_2"; vrsta = Vaje; trajanje = 1}];
  [];
  [{predmet = "sre_pred_1"; vrsta = Predavanje; trajanje = 1}];
  [];
  [];
  [{predmet = "sob_vaje_1"; vrsta = Vaje; trajanje = 1}]
]

let je_preobremenjen urnik =
  let rec je_preobremenjen' acc_v acc_p = function (* sprejme dan *)
    | [] -> (acc_v > 4) || (acc_p > 4)
    | {predmet; vrsta; trajanje} :: xs when vrsta = Vaje -> je_preobremenjen' (acc_v + trajanje) acc_p xs
    | {predmet; vrsta; trajanje} :: xs -> je_preobremenjen' acc_v (acc_p + trajanje) xs
  in
  let preveri_dan dan = je_preobremenjen' 0 0 dan
  in
  let rec glavna = function
    | [] -> false
    | dan :: ostali when preveri_dan dan -> true
    | dan :: ostali -> glavna ostali
  in
  glavna urnik



let rec stakni acc = function (* Funkcija repno rekurzivno stakne dva seznama, ko vrstni red ni pomemben!*)
  | [] -> acc
  | x :: xs -> stakni (x :: acc) xs

let bogastvo urnik =
  let vrednost ura =
  match (ura.vrsta, ura.trajanje) with
    | (Vaje, x) -> x
    | (Predavanje, x) -> 2 * x
  in
  let rec bogastvo' placilo =  function (* (dnevi, ure) *)
      | ([], []) -> placilo
      | ([], ura :: xs) -> bogastvo' ((vrednost ura) + placilo) ([], xs)
      | ([] :: xs, ure) -> bogastvo' placilo (xs, ure)
      | ((ura :: rest) :: xs, ure) -> bogastvo' ((vrednost ura) + placilo) (xs,(stakni rest ure))
  in
  bogastvo' 0 (urnik, [])