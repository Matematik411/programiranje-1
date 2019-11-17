(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)
let sestej = (+)

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)
let pristej_tri = (+) 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)
let vsem_pristej_pet = 
  let rec vsem_pristej_pet' acc = function
    | [] -> List.rev acc
    | x :: xs -> vsem_pristej_pet' ((sestej x 5) :: acc) xs
  in
  vsem_pristej_pet' [] 

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)
let tretji (a, b, c) = c

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)
let kompozitum f g x = g (f x)
 

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = Roza of 'a * 'a drevo list

let t = Roza (1,[])
let t' = Roza (2,[t;t;t;t;t;t])
let t'' = Roza (3,[Roza(-1,[]); t'; Roza(0,[])])

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)
let koren (Roza (a, _)) = a

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)
let rec stakni acc = function
  | [] -> acc
  | x :: xs -> stakni (x :: acc) xs

let kaksno_negativno drevo =
  let rec kaksno_negativno' = function (* (trenutno, preostala) *)
    | (Roza (a,[]),[]) -> a < 0
    | (Roza (a, x :: xs), preostala) when a >= 0 -> kaksno_negativno' (x, stakni xs preostala)
    | (Roza (a, x :: xs), _) -> true
    | (Roza (a, []), x :: xs) when a >= 0 -> kaksno_negativno' (x, xs)
    | (Roza (a, []), x :: xs) -> true
  in
  kaksno_negativno' (drevo, [])

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)
let drevo_z_veliko_otroci n =
  let rec seznam acc k = 
    if k > 0 then seznam ((Roza (0, [])) :: acc) (k - 1) else acc
  in
  Roza (0, (seznam [] n))



(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let velikost drevo = 
  let rec velikost' stevilo = function (* (trenutno, preostala) *)
    | (Roza (_, []), []) -> stevilo + 1
    | (Roza (_, x :: xs), preostala) -> velikost' (stevilo + 1) (x, stakni xs preostala)
    | (Roza (_, []), x :: xs) -> velikost' (stevilo + 1) (x, xs)
  in
  velikost' 0 (drevo, [])
