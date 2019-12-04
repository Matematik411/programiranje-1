(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree


let leaf x = Node (Empty, x, Empty)

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

let test_tree = Node (
  Node (leaf 0, 2, Empty),
  5,
  Node (leaf 6, 7, leaf 11)
)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)
let rec insert a = function
  | Empty -> leaf a
  | Node (l, x, r) ->
      if a = x then Node (l, x, r)
      else
      if a < x then Node (insert a l, x, r)
      else
      Node (l, x, insert a r)

let bst_of_list = 
  let rec bst' acc = function
    | [] -> acc
    | x :: xs -> bst' (insert x acc) xs
  in
  bst' Empty


(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)
let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)

let tree_sort list =
  list_of_tree (bst_of_list list)

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)
type direction = 
  | Left
  | Right

let rec follow directions tree = 
  match directions, tree with
    | _, Empty -> None
    | [], Node (_, x, _) -> Some x
    | d :: ds, Node (l, x, r) when d = Left -> follow ds l
    | d :: ds, Node (l, x, r) -> follow ds r
(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)
let rec prune directions tree = 
  match directions, tree with
    | [], _ -> Some Empty
    |  _, Empty -> None
    | d :: [], Node (l, x, r) when d = Left -> Some (Node (Empty, x, r))
    | d :: [], Node (l, x, r) -> Some (Node (l, x, Empty))
    | d :: ds, Node (l, x, r) when d = Left -> prune ds l
    | d :: ds, Node (l, x, r) -> prune ds r

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type state = 
  | Exists
  | Ghost

type 'a phantom_tree = 
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state
(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)
let rec phantomize = function
  | Empty -> P_Empty
  | Node (l, x, r) -> P_Node (phantomize l, x, phantomize r, Exists)

let rec kill a = function
  | P_Empty -> P_Empty
  | P_Node (l, x, r, s) when a > x -> P_Node (l, x, kill a r, s)
  | P_Node (l, x, r, s) when a < x -> P_Node (kill a l, x, r, s)
  | P_Node (l, x, r, s) -> P_Node (l, x, r, Ghost)
(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)
let unphantomize ptree = 
  let rec to_list = function
    | P_Empty -> []
    | P_Node (l, x, r, s) when s = Exists -> (to_list l) @ [x] @ (to_list r)
    | P_Node (l, x, r, s) -> (to_list l) @ (to_list r)
  in
  ptree |> to_list |> bst_of_list