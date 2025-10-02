(*----------------------------------------------------------------------------*
 # Rekurzija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite spodaj opisane funkcije, ne da bi uporabljali funkcije iz standardne
 knjižnice. Kjer to kažejo primeri, napišite tudi repno rekurzivne različice z
 imenom `ime_funkcije_tlrec`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Funkcija `reverse`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte pomožno funkcijo za obračanje seznamov. Funkcija naj bo repno
 rekurzivna.
[*----------------------------------------------------------------------------*)

let reverse = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] 

let primer_reverse = reverse [1; 2; 3; 4; 5]
(* val primer_reverse : int list = [5; 4; 3; 2; 1] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `repeat`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `repeat x n` vrne seznam `n` ponovitev vrednosti `x`. Za neprimerne
  vrednosti `n` funkcija vrne prazen seznam.
[*----------------------------------------------------------------------------*)

let rec repeat x = function
  | n when n <= 0 -> []
  | n -> x :: repeat x (n - 1)

let primer_repeat_1 = repeat "A" 5
(* val primer_repeat_1 : string list = ["A"; "A"; "A"; "A"; "A"] *)

let primer_repeat_2 = repeat "A" (-2)
(* val primer_repeat_2 : string list = [] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `range`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `range` naj sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente naj funkcija vrne prazen
 seznam. Funkcija naj bo repno rekurzivna. Pri tem ne smete uporabiti vgrajene
 funkcije `List.init`.
[*----------------------------------------------------------------------------*)

let rec range_not_tr = function
  | n when n < 0 -> []
  | n -> (range_not_tr (n-1)) @ [n]

let range =
  let rec aux acc m =
    match m with
    | m when m < 0 -> acc
    | m -> aux (m :: acc) (m - 1)
  in
  aux [] 

let primer_range = range_not_tr 10
let primer_range_tr = range 10
(* val primer_range : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `map`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `map f list` naj sprejme seznam `list` oblike `x0; x1; x2; ...` in
 funkcijo `f` ter vrne seznam preslikanih vrednosti, torej `f x0; f x1; f x2;
 ...`. Pri tem ne smete uporabiti vgrajene funkcije `List.map`.
[*----------------------------------------------------------------------------*)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs

let primer_map_1 =
  let plus_two = (+) 2 in
  map plus_two [0; 1; 2; 3; 4]
(* val primer_map_1 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 Napišite še funkcijo `map_tlrec`, ki naj bo repno rekurzivna različica funkcije
 `map`.
[*----------------------------------------------------------------------------*)

let map_tlrec f = 
  let rec aux acc = function
    | [] -> reverse acc
    | x :: xs -> aux (f x :: acc) xs
  in
  aux []

let primer_map_2 =
  let plus_two = (+) 2 in
  map_tlrec plus_two [0; 1; 2; 3; 4]
(* val primer_map_2 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `mapi` naj bo ekvivalentna Python kodi:

 ```python
 def mapi(f, lst):
     mapi_lst = []
     index = 0
     for x in lst:
         mapi_lst.append(f(index, x))
         index += 1
     return mapi_lst
 ```

 Pri tem ne smete uporabiti vgrajene funkcije `List.mapi`.
[*----------------------------------------------------------------------------*)

let mapi f =  
  let rec aux acc i = function
    | [] -> reverse acc
    | x :: xs -> aux ((f i x) :: acc) (i+1) xs
  in
  aux [] 0

let primer_mapi = mapi (+) [0; 0; 0; 2; 2; 2]
(* val primer_mapi : int list = [0; 1; 2; 5; 6; 7] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `zip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `zip` naj sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine, naj vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije `List.combine`.
[*----------------------------------------------------------------------------*)

let zip xs ys = 
  let rec aux acc = function
    | ([], []) -> List.rev acc
    | ([], _) | (_, []) -> failwith "seznama razlicnih dolzin"
    | (a :: aa, b :: bb) -> aux ((a, b) :: acc) (aa, bb)
  in
  aux [] (xs, ys)

let primer_zip_1 = zip [1; 1; 1; 1] [0; 1; 2; 3]
(* val primer_zip_1 : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)] *)

(* let primer_zip_2 = zip [1; 1; 1; 1] [1; 2; 3; 4; 5] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `unzip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `unzip` je inverz funkcije `zip`, torej sprejme seznam parov
  `(x0, y0); (x1, y1); ...` in vrne par seznamov (`x0; x1; ...`, `y0; y1; ...`).
  Pri tem ne smete uporabiti vgrajene funkcije `List.split`.
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> ([], [])
  | (a, b) :: rest -> 
      let (left, right) = unzip rest in
      (a :: left, b :: right)
  
let primer_unzip_1 = unzip [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_1 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*----------------------------------------------------------------------------*
 Funkcija `unzip_tlrec` je repno rekurzivna različica funkcije `unzip`.
[*----------------------------------------------------------------------------*)

let unzip_tlrec pairs = 
  let rec aux lacc racc = function
    | [] -> (List.rev lacc, List.rev racc)
    | (a, b) :: rest -> aux (a::lacc) (b::racc) rest
  in
    aux [] [] pairs

let primer_unzip_2 = unzip_tlrec [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_2 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*----------------------------------------------------------------------------*
 ## Zanke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `loop condition f x` naj se izvede kot python koda:

 ```python
 def loop(condition, f, x):
     while condition(x):
         x = f(x)
     return x
 ```
[*----------------------------------------------------------------------------*)

let rec loop cond f x = 
  if cond x then loop cond f (f x) else x

let primer_loop = loop (fun x -> x < 10) ((+) 4) 4
(* val primer_loop : int = 12 *)

(*----------------------------------------------------------------------------*
 ## Funkcija `fold_left`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `fold_left_no_acc f list` naj sprejme seznam `x0; x1; ...; xn` in
 funkcijo dveh argumentov `f` in vrne vrednost izračuna `f(... (f (f x0 x1) x2)
 ... xn)`. V primeru seznama z manj kot dvema elementoma naj vrne napako.
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f = function
  | [] | [_] -> failwith "prekratek seznam"
  | sl:: [l] -> f sl l
  | x :: xs -> f x (fold_left_no_acc f xs)

let primer_fold_left_no_acc =
  fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"]
(* val primer_fold_left_no_acc : string = "FICUS" *)

(*----------------------------------------------------------------------------*
 ## Funkcija `apply_sequence`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `apply_sequence f x n` naj vrne seznam zaporednih uporab funkcije `f`
 na vrednosti `x` do vključno `n`-te uporabe, torej `[x; f x; f (f x); ...; fⁿ
 x]`. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let apply_sequence f x n = 
  let rec aux acc y = function
    | n when n <= 0 -> List.rev acc
    | n -> aux (f y :: acc) (f y) (n-1)
  in 
  aux [] x n

let primer_apply_sequence_1 = apply_sequence (fun x -> x * x) 2 5
(* val primer_apply_sequence_1 : int list = [2; 4; 16; 256; 65536; 4294967296] *)

let primer_apply_sequence_2 = apply_sequence (fun x -> x * x) 2 (-5)
(* val primer_apply_sequence_2 : int list = [] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `filter`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `filter f list` vrne seznam elementov `list`, pri katerih funkcija `f`
  vrne vrednost `true`.
  Pri tem ne smete uporabiti vgrajene funkcije `List.filter`.
[*----------------------------------------------------------------------------*)

let rec filter f = function
  | [] -> []
  | x :: xs when f x -> x :: filter f xs
  | x :: xs -> filter f xs

let primer_filter = filter ((<)3) [0; 1; 2; 3; 4; 5]
(* val primer_filter : int list = [4; 5] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `exists`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `exists` sprejme seznam in funkcijo, ter vrne vrednost `true` čim
  obstaja element seznama, za katerega funkcija vrne `true` in `false` sicer.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let filter_tr f xs =
  let rec aux acc = function
    | [] -> List.rev acc
    | y :: ys -> if f y then aux ((f y) :: acc) ys else aux acc ys
  in
  aux [] xs

let exists f xs = 
  let non_empty = function
    | [] -> false
    | _ -> true
  in
  non_empty (filter_tr f xs)

let rec exists_directly f = function
  | [] -> false
  | x :: xs -> if f x then true else exists_directly f xs

let primer_exists_1 = exists ((<) 3) [0; 1; 2; 3; 4; 5]
(* val primer_exists_1 : bool = true *)

let primer_exists_3 = exists_directly ((<) 3) [0; 1; 2; 3; 4; 5]
(* val primer_exists_3 : bool = true *)

let primer_exists_2 = exists ((<) 8) [0; 1; 2; 3; 4; 5]
(* val primer_exists_2 : bool = false *)

(*----------------------------------------------------------------------------*
 ## Funkcija `first`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `first f default list` vrne prvi element seznama, za katerega
  funkcija `f` vrne `true`. Če takšnega elementa ni, vrne `default`.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let rec first f default = function
  | [] -> default
  | x :: xs -> if f x then x else first f default xs

let primer_first_1 = first ((<) 3) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_1 : int = 5 *)

let primer_first_2 = first ((<) 8) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_2 : int = 0 *)
