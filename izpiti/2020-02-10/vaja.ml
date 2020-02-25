let test = function
  | 0 -> "a" ^ string_of_int 2
  | n -> "ab"

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let drevo = Node (Empty, 5, Node (Empty, 3, Empty))

let rec foldleft_tree f acc = function
  | Empty -> acc
  | Node (l, x, r) -> (let left = foldleft_tree f acc l in
                      let middle = f left x in
                      foldleft_tree f middle r)

let rec foldright_tree f tree acc = 
  match tree with
  | Empty -> acc
  | Node (l, x, r) -> (let right = foldright_tree f r acc in
                      let middle = f x right  in
                      foldright_tree f l middle)

let testni = foldleft_tree (fun x y -> x + y) 0 drevo
let testni_r = foldright_tree (fun x y -> x + y) drevo 0 