(*----------------------------------------------------------------------------*
 # Iskalna drevesa: dodatne naloge
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 V dodatnih nalogah uporabljamo tip dreves in testno drevo iz vaj ter funkcije
 `map_tree`, `list_of_tree` in `insert`. Namesto spodnjih nastavkov
 prekopirajte svoje rešitve iz vaj.
[*----------------------------------------------------------------------------*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test_tree = Node (Node (leaf 0, 2, Empty), 5, Node (leaf 6, 7, leaf 11))

let rec map_tree f = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> list_of_tree l @ [x] @ list_of_tree r

let rec insert x = function
  | Empty -> leaf x
  | Node (l, y, r) when x < y -> Node (insert x l, y, r)
  | Node (l, y, r) when x > y -> Node (l, y, insert x r)
  | tree -> tree

(*----------------------------------------------------------------------------*
 ## Urejanje z drevesi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `tree_sort` uredi seznam tako, da njegove elemente enega za drugim
 vstavi v prazno iskalno drevo, nato pa drevo pretvori nazaj v seznam.

 Kaj se zgodi, če se elementi v seznamu ponavljajo? Kakšna je časovna
 zahtevnost funkcije na že urejenem seznamu? Bi pomagalo, če bi namesto
 `insert` uporabili `avl_insert` iz vaj?

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
[*----------------------------------------------------------------------------*)

let tree_sort list =
  list |> List.fold_left (fun tree x -> insert x tree) Empty |> list_of_tree

let primer_tree_sort = tree_sort ["a"; "c"; "f"; "b"; "e"; "d"]
(* val primer_tree_sort : string list = ["a"; "b"; "c"; "d"; "e"; "f"] *)

(*
 Funkcija `insert` elementa, ki je že v drevesu, ne doda še enkrat, zato se
 ponovitve izgubijo, npr. `tree_sort [3; 1; 3; 2]` vrne `[1; 2; 3]`. Po
 definiciji iz prejšnjih vaj funkcija torej ne uredi seznama, saj rezultat ni
 permutacija vhoda. Temu bi se lahko izognili tako, da bi v vozliščih hranili
 še število ponovitev elementa.

 Če je seznam že urejen, vsak element vstavimo desno od vseh prejšnjih, zato
 je drevo v resnici seznam. Pri vstavljanju `i`-tega elementa se tako
 sprehodimo čez `i - 1` vozlišč, kar skupaj nanese O(n^2) časa.

 Z `avl_insert` drevo ostane uravnoteženo, vendar naša funkcija `balance` v
 vsakem vozlišču na poti znova izračuna višini poddreves. Že v korenu to stane
 O(n) časa, zato je tudi tako urejanje O(n^2). Šele če višine hranimo v
 vozliščih, kot pri zadnji implementaciji AVL dreves s predavanj, vstavljanje
 porabi O(log n) časa, urejanje pa O(n log n).
*)

(*----------------------------------------------------------------------------*
 ## Navodila za pot

 Pot po drevesu opišemo s seznamom navodil tipa `direction`, ki povedo, ali se
 iz vozlišča premaknemo v levo ali v desno poddrevo.
[*----------------------------------------------------------------------------*)

type direction = Left | Right

(*----------------------------------------------------------------------------*
 Funkcija `follow directions tree` tipa `direction list -> 'a tree -> 'a option`
 sprejme seznam navodil za premikanje po drevesu in vrne podatek v vozlišču, do
 katerega vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v
 drevesu, vrne rezultat kot `option` tip.
[*----------------------------------------------------------------------------*)

let rec follow directions tree =
  match (directions, tree) with
  | _, Empty -> None
  | [], Node (_, x, _) -> Some x
  | Left :: rest, Node (l, _, _) -> follow rest l
  | Right :: rest, Node (_, _, r) -> follow rest r

let primer_follow = follow [Right; Left] test_tree
(* val primer_follow : int option = Some 6 *)

let primer_follow_2 = follow [Right; Left; Right; Right] test_tree
(* val primer_follow_2 : int option = None *)

(*----------------------------------------------------------------------------*
 Funkcija `prune directions tree` poišče vozlišče v drevesu glede na navodila
 in izbriše poddrevo, ki se začne v izbranem vozlišču. Če navodila ne vodijo do
 nobenega vozlišča, vrne `None`.

 Opozorilo: Pri uporabi `Some Node (l, x, r)` se OCaml pritoži, saj to razume
 kot `(Some Node) (l, x, r)`, zato pravilno postavite potrebne oklepaje.
[*----------------------------------------------------------------------------*)

let rec prune directions tree =
  match (directions, tree) with
  | _, Empty -> None
  | [], Node _ -> Some Empty
  | Left :: rest, Node (l, x, r) -> (
      match prune rest l with
      | None -> None
      | Some l' -> Some (Node (l', x, r)))
  | Right :: rest, Node (l, x, r) -> (
      match prune rest r with
      | None -> None
      | Some r' -> Some (Node (l, x, r')))

let primer_prune = prune [Right] test_tree
(* val primer_prune : int tree option =
     Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty)) *)

let primer_prune_2 = prune [Left; Right] test_tree
(* val primer_prune_2 : int tree option = None *)

(*----------------------------------------------------------------------------*
 ## Leno brisanje

 Brisanje iz iskalnega drevesa je zapleteno, saj moramo drevo preurediti, pri
 AVL drevesih pa še uravnotežiti. Druga možnost je leno brisanje, pri katerem
 vozlišča ne odstranimo, temveč ga le označimo kot izbrisano. Tako dobimo
 fantomsko vozlišče, ki ga še vedno uporabljamo pri iskanju po drevesu, med
 elemente drevesa pa ga ne štejemo več.

 Fantomsko drevo predstavimo kot drevo parov `(x, exists)`, kjer `exists` pove,
 ali je element `x` še v drevesu. Še vedno predpostavljamo, da imajo drevesa
 glede na elemente `x` strukturo BST.
[*----------------------------------------------------------------------------*)

type 'a phantom_tree = ('a * bool) tree

(*----------------------------------------------------------------------------*
 Funkcija `phantomize` navadnemu drevesu priredi ekvivalentno fantomsko drevo,
 v katerem so vsi elementi še prisotni. Funkcija `kill x ptree` iz fantomskega
 drevesa izbriše element `x` tako, da ga označi kot izbrisanega. Če elementa ni
 v drevesu, drevo ostane nespremenjeno.

 Namig: Ali lahko uporabite katero od funkcij iz vaj?
[*----------------------------------------------------------------------------*)

let phantomize tree : 'a phantom_tree = map_tree (fun x -> (x, true)) tree

let rec kill x (ptree : 'a phantom_tree) : 'a phantom_tree =
  match ptree with
  | Empty -> Empty
  | Node (l, (y, exists), r) ->
      if x < y then Node (kill x l, (y, exists), r)
      else if x > y then Node (l, (y, exists), kill x r)
      else Node (l, (y, false), r)

let primer_phantomize = phantomize test_tree
(* val primer_phantomize : int phantom_tree =
     Node (Node (Node (Empty, (0, true), Empty), (2, true), Empty), (5, true),
      Node (Node (Empty, (6, true), Empty), (7, true),
       Node (Empty, (11, true), Empty))) *)

let primer_kill = test_tree |> phantomize |> kill 7 |> kill 3
(* val primer_kill : int phantom_tree =
     Node (Node (Node (Empty, (0, true), Empty), (2, true), Empty), (5, true),
      Node (Node (Empty, (6, true), Empty), (7, false),
       Node (Empty, (11, true), Empty))) *)

(*----------------------------------------------------------------------------*
 Funkcija `unphantomize` fantomskemu drevesu priredi navadno iskalno drevo, ki
 vsebuje zgolj elemente, ki še niso izbrisani. Oblika novega drevesa ni
 pomembna.

 Namig: Lahko uporabite vmesni prehod na drugo podatkovno strukturo.
[*----------------------------------------------------------------------------*)

let unphantomize (ptree : 'a phantom_tree) =
  ptree
  |> list_of_tree
  |> List.filter (fun (_, exists) -> exists)
  |> List.map fst
  |> List.fold_left (fun tree x -> insert x tree) Empty

let primer_unphantomize =
  test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize
  |> list_of_tree
(* val primer_unphantomize : int list = [2; 6; 11] *)

(*----------------------------------------------------------------------------*
 Fantomska vozlišča zasedajo prostor in podaljšujejo iskanje, zato drevo
 občasno s funkcijo `unphantomize` zgradimo na novo. Kdaj se to splača?
 Kakšne oblike je drevo, ki ga vrne vaša funkcija `unphantomize`?
[*----------------------------------------------------------------------------*)

(*
 Leno brisanje drevesa ne preureja, zato drevo po brisanju ostane enako
 visoko. Če je drevo z `n` vozlišči, od katerih je `m` neizbrisanih,
 uravnoteženo, iskanje porabi O(log n) časa. Dokler je izbrisanih največ
 polovica vozlišč, je `n <= 2m`, zato je to še vedno O(log m). Drevo se tako
 splača zgraditi na novo, ko je izbrisanih približno polovica vozlišč. Takrat
 smo od prejšnje gradnje izbrisali že veliko elementov, zato se cena gradnje
 porazdeli med vsa ta brisanja.

 Seznam, ki ga vrne `list_of_tree`, je urejen, zato z zaporednim vstavljanjem
 z `insert` dobimo drevo, ki je v resnici seznam. Bolje je, da za koren vzamemo
 srednji element seznama, iz levega in desnega dela pa rekurzivno zgradimo
 levo in desno poddrevo. Tako dobimo uravnoteženo drevo.
*)

let rec balanced_of_sorted_list = function
  | [] -> Empty
  | list -> (
      let rec split k = function
        | x :: rest when k > 0 ->
            let left, right = split (k - 1) rest in
            (x :: left, right)
        | rest -> ([], rest)
      in
      match split (List.length list / 2) list with
      | left, x :: right ->
          Node (balanced_of_sorted_list left, x, balanced_of_sorted_list right)
      | _, [] -> failwith "Ne more se zgoditi")

let unphantomize' (ptree : 'a phantom_tree) =
  ptree
  |> list_of_tree
  |> List.filter (fun (_, exists) -> exists)
  |> List.map fst
  |> balanced_of_sorted_list

let primer_unphantomize' =
  test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize'
(* val primer_unphantomize' : int tree =
     Node (Node (Empty, 2, Empty), 6, Node (Empty, 11, Empty)) *)
