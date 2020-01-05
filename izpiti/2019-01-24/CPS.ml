type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree 

let test = Node (Node (Empty, 5, Node (Empty, 4, Node (Empty, 2, Empty))), 3, Node (Empty, 1, Empty))


let depth tree = 
  let rec aux k = function
    | Empty -> k 0
    | Node (l, _, r) ->
      aux (fun lx ->
      aux (fun rx -> k (1 + max lx rx)) r
      ) l

  in
  aux (fun x -> x) tree