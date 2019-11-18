(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame tri cela števila ter vrne njihov produkt.
   Primer: /zmnozi 2 3 4 = 24/ *)
let zmnozi a b c = a * b * c 

(* 1.2) Definirajte funkcijo, ki vzame celo število x in celo število k, ter
   vrne vrednost izraza x^3 + k.
   Primer: /afin_kub 2 1 = 9/ *)
let afin_kub x k = x * x * x + k

(* 1.3) Definirajte funkcijo, ki vzame seznam in izračuna seznam vrednosti funkcije
   f(x) = x^3 + 2 za elemente vhodnega seznama.
   Primer: /vse_kubiraj_in_pristej_dva [1; 2; 3] = [3; 10; 29]/ *)
let vse_kubiraj_in_pristej_dva = 
  let rec vse_kubiraj_in_pristej_dva' acc = function
    | [] -> List.rev acc
    | x :: xs -> vse_kubiraj_in_pristej_dva' ((afin_kub x 2) :: acc) xs
  in
  vse_kubiraj_in_pristej_dva' [] 

(* 1.4) Definirajte funkcijo, ki varno vrne zadnji element seznama v primeru,
   da seznam ni prazen. Uporabite tip option.
   Primer: /zadnji_element [1; 2; 3] = Some 3/ *)
let zadnji_element list = 
  match (List.rev list) with
  | [] -> None
  | x :: xs -> Some x

(* 1.5) Definirajte funkcijo, ki izračuna n-to Fibonaccijevo število.
   Pri tem upoštevamo začetna pogoja /fibonacci 0 = 1/ in /fibonacci 1 = 1/.
   Primer: /fibonacci 20 = 10946/ *)
let fibonacci n = 
  let rec fibonacci' a b n = if n > 0 then fibonacci' b (a + b) (n - 1) else a
  in
  fibonacci' 1 1 n

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = Roza of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Roza (1,[])
let t' = Roza (2,[t;t;t;t;t;t])
let t'' = Roza (3,[Roza(-1,[]); t'; Roza(0,[])])

(* 2.3) Definirajte funkcijo, ki preveri ali je dano rožno drevo list drevesa,
   torej ima prazen gozd poddreves. *)
let je_list (Roza (_, gozd)) = gozd = []

(* 2.4) Definirajte funkcijo, ki preveri, ali drevo celih števil vsebuje zgolj pozitivna števila. *)
let rec stakni acc = function
  | [] -> acc
  | x :: xs -> stakni (x :: acc) xs

let vsa_pozitivna drevo = 
  let rec vsa_pozitivna' (trenuten, rest) = 
    match (trenuten, rest) with
      | (Roza (a, []), []) -> a > 0
      | (Roza (a, []), x :: xs) -> if a > 0 then vsa_pozitivna' (x, xs) else false
      | (Roza (a, x :: xs), _) -> if a > 0 then vsa_pozitivna' (x, (stakni xs rest)) else false
  in
  vsa_pozitivna' (drevo, [])
      

(* 2.5) Definirajte funkcijo, ki izračuna največjo širino rožnega drevesa, torej največjo dolžino
   gozda, ki se pojavi v kateremkoli vozlišču rožnega drevesa. *)

let rec sirina_drevesa (Roza(_, gozd)) = 
  let rec sirina' acc = function
    | Roza (_, []) -> max acc 0
    | Roza (_, x :: xs) -> sirina' (max (sirina' 0 x) (acc + 1)) (Roza (0, xs))
  in 
  sirina' 0 (Roza(0, gozd))

let rec fold_sirina (Roza(_, gozd)) = 
  List.map fold_sirina gozd |> List.fold_left max (List.length gozd) 

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let globoko_drevo = 
  let rec gradi drevo = function
    | 1 -> drevo
    | n -> gradi (Roza (0,[drevo])) (n - 1)
  in
  gradi (Roza (0,[]))

(* 2.7) Definirajte funkcijo, ki pretvori rožno drevo v seznam. Vrstni red vrednosti v seznamu
   pri tem ni pomemben.
   Primer: /drevo_v_seznam t'' = [3; -1; 2; 1; 1; 0]/ (ali katerakoli permutacija [3; -1; 2; 1; 1; 0])

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let drevo_v_seznam drevo = 
  let rec drevo_v_seznam' (trenutno, rest) seznam =
    match (trenutno, rest) with
      | (Roza (a, []), []) -> a :: seznam
      | (Roza (a, []), x :: xs) -> drevo_v_seznam' (x, xs) (a :: seznam)
      | (Roza (a, x :: xs), _) -> drevo_v_seznam' (x, (stakni xs rest)) (a :: seznam)
  in
  drevo_v_seznam' (drevo, []) []

let drevo_v_seznam_OG t =
  let rec aux (acc : 'a list) (stack : 'a drevo list list) =
    (* Poglej če imamo še kakšen gozd za obdelavo.*)
    match stack with
    | [] -> acc
    | ts :: stack ->
      (* Poglej ali je v gozdu še kakšno drevo.*)
      (match ts with
       | [] -> aux acc stack
       | (Rose (root, forest)) :: ts ->
         (* Dodaj koren v akumulator in gozd v stack. *)
          aux (root :: acc) (forest :: ts :: stack))
 in aux [] [[t]] |> List.rev