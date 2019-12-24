(* ========== Vaje 6: Dinamično programiranje  ========== *)
(* DELAMO OD SPODAJ GOR - "BOTTOM UP" *)

(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

(* sestavili bomo matriko, koliko sirov lahko max poberemo od tega polja naprej *)

let max_cheese cheese_matrix =
  let height = Array.length cheese_matrix in
  let width = 
    if height = 0 then failwith "matrix has no elements"
    else 
    Array.length cheese_matrix.(0) in
  let sum_matrix = Array.make_matrix height width 0

  in
  
  let eat i j = 
    let cheese = cheese_matrix.(i).(j) in
    if i < (height - 1) then
      if j < (width - 1) then
        cheese + (max sum_matrix.(i).(j+1) sum_matrix.(i+1).(j))
      else
        cheese + sum_matrix.(i+1).(j)
    else
      if j < (width - 1) then
        cheese + sum_matrix.(i).(j+1)
      else
        cheese
  in


  (* (* še ena možnost *)
  let eat2 i j =
    let cheese = cheese_matrix.(i).(j) in
    let max_right = if j < (width - 1) then sum_matrix.(i).(j+1) else 0 in
    let max_down = if i < (height - 1) then sum_matrix.(i+1).(j) else 0 in 
    cheese + max max_down max_right 
  in *)


  let rec loop i j = 
    let cheese = eat i j in
    let () = sum_matrix.(i).(j) <- cheese in
    if j > 0 then
      loop i (j - 1)
    else
      (* nova vrstica *)
      if i > 0 then
        loop (i - 1) (width - 1)
      else
        ()
  in
  let () = loop (height - 1) (width - 1) in
  sum_matrix.(0).(0)


(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)
type color =
  | Red
  | Blue

let alternating_towers n = 
  (* shema dinamičnega programiranja *)
  (* Establish bounds *)
  (* Make memory *)
  (* Calculate one value by using recursion with memory *)
  (* Loop over all values in the correct order *)
  (* Return result *)
  (* The end *)
  let red_towers = Array.make (n+1) 0 in
  let blue_towers = Array.make (n+1) 0 in

  let towers m color = 
    if m = 0 then 0 else
    match color with
      | Red when m <= 2 -> 1
      | Red -> blue_towers.(m-1) + blue_towers.(m-2)
      | Blue when m = 1 -> 0
      | Blue when m <= 2 -> 1
      | Blue when m = 3 -> 2
      | Blue -> red_towers.(m-2) + red_towers.(m-3)

  in
  let rec loop i = 

    let nr_red = towers i Red in
    let nr_blue = towers i Blue in
    let () = red_towers.(i) <- nr_red in
    let () = blue_towers.(i) <- nr_blue in

    if i < n then
      loop (i + 1)
    else ()

  in

  let () = loop 0 in
  red_towers.(n) + blue_towers.(n)

(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.

 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]

let best_value articles max_w =
  (* Choose the item if you can and recursively search further. *)
  let rec get_item acc_w acc_p (name, p, w) =
    if acc_w +. w > max_w then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item. *)
      shopper (acc_w +. w) (acc_p +. p)
  (* Choose every item in the list and return the value of the best choice. *)
  and shopper w p =
    let choices = Array.map (get_item w p) articles in
    Array.fold_left max 0. choices
  in
  shopper 0. 0.


let best_value_unique articles max_w =
  (* Store which items have already been chose in the array [taken]. *)
  (* Choose the item if you can and recursively search further. *)
  let rec get_item taken acc_w acc_p i (_, p, w) =
    if acc_w +. w > max_w || taken.(i) then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item, mark taking the item in [taken]. *)
      let new_taken = Array.copy taken in
      let () = new_taken.(i) <- true in
      shopper new_taken (acc_w +. w) (acc_p +. p)
  (* Choose every item in the list and return the value of the best choice. *)
  and shopper taken w p =
    let choices = Array.mapi (get_item taken w p) articles in
    Array.fold_left max 0. choices
  in

  let taken = Array.map (fun _ -> false) articles in
  shopper taken 0. 0.




(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite vrednost najdražjega sprehoda od korena do listov drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path Empty ;;
 - : 'a option = None
 # max_path test_tree;;
- : int option = Some 21
[*----------------------------------------------------------------------------*)

type 'a tree =
 | Empty
 | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree =     Node( 
          Node(leaf 0,    2,    leaf 13),     5,      Node(leaf 9,    7,     leaf 4))



let rec max_path tree =
  let vecji = function (* matching (a, b) *)
    | (None, Some b) -> b
    | (Some a, None) -> a
    | (Some a, Some b) -> max a b
    | _ -> failwith "napaka"
  in
  match tree with 
  | Empty -> None
  | Node (Empty, x, Empty) -> Some x
  | Node (left, x, right) -> Some (x + vecji ((max_path left), (max_path right)))

let _ = max_path test_tree;;
(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite najdražji sprehod od korena do listov drevesa: Funkcija pot vrne v 
 obliki seznama smeri, katere je potrebno izbrati za najdražji sprehod.

 Napišite tudi funkcijo, ki sprehod pretvori v elemente sprehoda
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path_trace Empty ;;
 - : 'a list = []
 # max_path_trace test_tree;;
- : direction list = [Right, Left]
 # reconstruct test_tree (max_path_trace test_tree);;
- : int list = [5; 7; 9]
[*----------------------------------------------------------------------------*)

type direcion =
  | Left
  | Right


let rec max_path_trace = function
  | Empty | Node (Empty, _, Empty) -> []
  | Node (left, _, right) when (max_path left) >= (max_path right) -> Left :: (max_path_trace left)
  | Node (left, _, right) -> Right :: (max_path_trace right)

let rec reconstruct tree dir = 
  match (tree, dir) with
  | (Node (_, x, _), []) -> [x]
  | (Node (left, x, _), Left :: xs) -> x :: (reconstruct left xs)
  | (Node (_, x, right), Right :: xs) -> x :: (reconstruct right xs)
  | (_, _) -> failwith "error"



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* naloga iz eulerja za najcenejši sprehod  - problem 81 *)
let my_matrix = [|  [|131; 673; 234; 103; 18|];
                    [|201; 96;  342; 965; 150|];
                    [|630; 803; 746; 422; 111|];
                    [|537; 699; 497; 121; 956|];
                    [|805; 732; 524; 37;  331|]    |]


  (* shema dinamičnega programiranja *)
  (* Establish bounds *)
  (* Make memory *)
  (* Calculate one value by using recursion with memory *)
  (* Loop over all values in the correct order *)
  (* Return result *)
  (* The end *)

let min_path matrix =
  let h = Array.length matrix in
  let w = Array.length matrix.(0) in

  let values = Array.make_matrix h w 0 in

  let calculate i j =
    if i < (h - 1) then
      if j < (w - 1) then
        matrix.(i).(j) + (min values.(i).(j+1) values.(i+1).(j))
      else
        matrix.(i).(j) + values.(i+1).(j)
    else
      if j < (w - 1) then
        matrix.(i).(j) + values.(i).(j+1)
      else
        matrix.(i).(j)
  
  in
  let rec loop i j = 
    let () = values.(i).(j) <- calculate i j in
    if j > 0 then
      loop i (j - 1)
    else
      if i > 0 then
        loop (i - 1) (w - 1)
      else
        ()
  in
  let () = loop (h - 1) (w - 1) in
  values.(0).(0)



let min_path_triple matrix =
  let h = Array.length matrix in
  let w = Array.length matrix.(0) in

  let values = Array.make_matrix h w 0 in

  let smallest_up i j =
    let how_much = ref values.(i).(j+1) in
    let path = ref 0 in
    for k = 1 to i do
      path := !path + matrix.(i-k).(j);
      how_much := min !how_much (!path + values.(i-k).(j+1));
      () done;
    !how_much  
    
  in
  let smallest_down i j =
    let how_much = ref values.(i).(j+1) in
    let path = ref 0 in
    for k = 1 to (h - i - 1) do
      path := !path + matrix.(i+k).(j);
      how_much := min !how_much (!path + values.(i+k).(j+1));
      () done;
    !how_much  


  in
  let calculate i j =
    if j < (w - 1) then
      if i < (h - 1) then
        if i > 0 then
          matrix.(i).(j) + (min 
                                (min (smallest_down i j) (smallest_up i j)) 
                            values.(i).(j+1))
        else (* zgornja *)
          matrix.(i).(j) + (min values.(i).(j+1) (smallest_down i j))
      else (* spodnja *)
        matrix.(i).(j) + (min values.(i).(j+1) (smallest_up i j))
    
    else
        matrix.(i).(j)
  
  in
  let rec loop i j = 
    let () = values.(i).(j) <- calculate i j in
    if i > 0 then
      loop (i-1) j
    else
      if j > 0 then
        loop (h - 1) (j - 1)
      else
        ()
  in
  let () = loop (h - 1) (w - 1) in
  let resitev = ref values.(0).(0) in
  for k = 1 to (h - 1) do 
    resitev := min !resitev values.(k).(0) done;
  !resitev
  
  