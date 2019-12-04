(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)
(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)
let test_tree = Node (
  Node (leaf 0, 2, Empty),
  5,
  Node (leaf 6, 7, leaf 11)
)


(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)
let rec mirror = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (mirror r, x, mirror l)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)
let rec height = function
  | Empty -> 0
  | Node (l, _, r) -> max (height l) (height r) + 1

let rec size = function
  | Empty -> 0
  | Node (l, _, r) -> (size l) +(size r) + 1
(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)
let rec map_tree f = function
  | Empty -> Empty
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)
let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec is_bst_w_list tree = 
  let rec is_sorted = function
    | [] | _ :: [] -> true
    | x :: y :: xs when x > y -> false
    | x :: y :: xs -> is_sorted (y :: xs)
  in
  tree |> list_of_tree |> is_sorted

(* vsaka točka bo morala biti med spodnjo in zgornjo mejo *)

(* še za poljubno funkcijo *)
let cmp_bind cmp x = function
  | None -> true
  | Some y -> cmp x y

let inside_bounds cmp low x high = cmp_bind (<) x high && cmp_bind (>) x low
(* pač zato ker lahko je ta del *)


let check_bounds lower higher x = 
  match lower, higher with
    | None, None -> true
    | None, Some c -> x < c
    | Some a, None -> a < x
    | Some a, Some c -> (a < x) && (x < c)

let rec is_bst = 
  let rec is_bst' lower_acc higher_acc = function
    | Empty -> true
    | Node (l, x, r) -> 
        let this_ok = check_bounds lower_acc higher_acc x
        in
        let left_ok = is_bst' lower_acc (Some x) l in
        let right_ok = is_bst' (Some x) higher_acc r in
        this_ok && left_ok && right_ok
  in
  is_bst' None None 
(* ker ne vemo kje se nastavita meji, uporabimo SOME TYPE *)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec member a = function
  | Empty -> false
  | Node (l, x, r) ->
      if a = x then true
      else
      if a < x then member a l
      else
      member a r

let rec insert a = function
  | Empty -> leaf a
  | Node (l, x, r) ->
      if a = x then Node (l, x, r)
      else
      if a < x then Node (insert a l, x, r)
      else
      Node (l, x, insert a r)

(* let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  print_endline ("Porabljen čas: " ^ string_of_float (1000. *. (konec -. zacetek)) ^ "ms");
  y

let veliko = 
  let rec gradi acc = function
    | x when x < 10000000 -> gradi (Node (acc, x, acc)) (x + 1)
    | x -> acc
  in gradi Empty 0 *)


  (*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)
let rec member2 a = function
  | Empty -> false
  | Node (l, x, r) -> a = x || member a l || member a r


(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)
let succ tree =
  let rec smallest = function
    | Empty -> None
    | Node (Empty, x, _) -> Some x
    | Node (l, _, _) -> smallest l
  in  
  match tree with 
  | Empty -> None
  | Node (_, _, r) -> smallest r

let pred tree =
  let rec biggest = function
    | Empty -> None
    | Node (_, x, Empty) -> Some x
    | Node (_, _, r) -> biggest r
  in  
  match tree with 
  | Empty -> None
  | Node (l, _, _) -> biggest l
(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)
let rec delete a tree = 
  match tree with 
  | Empty -> Empty
  | Node (l, x, r) when a < x -> Node (delete a l, x, r)
  | Node (l, x, r) when a > x -> Node (l, x, delete a r)
  | Node (l, x, r) -> 
    match (succ tree) with
      | None -> l
      | Some s -> Node (l, s, delete s r)

let rec delete_pred a tree = 
  match tree with 
  | Empty -> Empty
  | Node (l, x, r) when a < x -> Node (delete a l, x, r)
  | Node (l, x, r) when a > x -> Node (l, x, delete a r)
  | Node (l, x, r) -> 
    match (pred tree) with
      | None -> l
      | Some s -> Node (delete s l, s, r)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa,ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
          "b":1
        /       \
    "a":0      "d":2
                /
            "c":-2
[*----------------------------------------------------------------------------*)
let test_dict = Node (
  leaf ("a", 0),
  ("b", 1),
  Node (leaf ("c", -2), ("d", 2), Empty)
)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)
let rec dict_get key = function
  | Empty -> None
  | Node (l, (k, v), r) when key < k -> dict_get key l
  | Node (l, (k, v), r) when key > k -> dict_get key r
  | Node (l, (k, v), r)-> Some v
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
let rec print_dict = function
  | Empty -> ()
  | Node (l, (k, v), r) -> 
      (print_dict l);
      print_string (k ^ " " ^ (string_of_int v) ^ "\n");
      (print_dict r) 

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
let rec dict_insert key value = function
  | Empty -> Node (Empty, (key, value), Empty)
  | Node (l, (k, v), r) when key < k -> Node (dict_insert key value l, (k, v), r)
  | Node (l, (k, v), r) when key > k -> Node (l, (k, v), dict_insert key value r)
  | Node (l, (k, v), r) -> Node (l, (key, value), r)