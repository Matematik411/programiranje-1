(* 1. naloga *)


(* a) *)
let dot_prod (x, y, z) (a, b, c) = x *. a +. y *. b +. z *. c

let a_1 = dot_prod (3.0, 3.5, 5.) (1., 2., 0.)


(* b) *)
let fix_second f b = (fun x y -> f y x) b
let fix_second_simple f b a = f a b

let b_1 = fix_second (fun x y -> x - y) 4 6
let b_1_2 = fix_second_simple (fun x y -> x - y) 4 6


(* c) *)
let combine_and_filter f xs ys =

  let rec combine_and_filter' acc = function
    | ([], _) | (_, []) -> List.rev acc (* list.rev je repno rek. *)
    | (x :: xrest, y :: yrest) -> 
        (match f x y with
        | None -> combine_and_filter' acc (xrest, yrest) 
        | Some z -> combine_and_filter' (z :: acc) (xrest, yrest))
  
  in
  
  combine_and_filter' [] (xs, ys)

let safe_minus x y = (if x > y then Some (x - y) else None)
let c_1 = combine_and_filter safe_minus [1;0;4;3] [2;1;0;2;5]


(* d) *)
let conditional_print f list = 
  
  (* pred vsakim razen prvim elementom naj izpiše tudi ", " *)
  let rec conditional_print' prvi = function 
    | [] -> ()

    | str :: rest when prvi -> 
      (if f str then 
        let () = print_string str in conditional_print' false rest 
      else conditional_print' prvi rest)

    | str :: rest -> 
      (if f str then 
        let () = print_string ", " in let () = print_string str in conditional_print' prvi rest 
      else conditional_print' prvi rest)
  in
  conditional_print' true list

(* ta testni primer zakomentiram, ker vsebuje print *)
let long_enough s = String.length s > 3
(* let d_1 = conditional_print long_enough ["Ta"; "izpit"; "je"; "neumen!"] *)


(* -----------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----------------------- *)
(* 2. naloga *)

type ('a, 'b) tree =
  | Empty
  | ANode of ('a, 'b) tree * 'a * ('a, 'b) tree
  | BNode of ('a, 'b) tree * 'b * ('a, 'b) tree

let aleaf x = ANode (Empty, x, Empty)
let bleaf x = BNode (Empty, x, Empty)


(* a) *)
let test = ANode (bleaf true, 12, ANode (aleaf 0, 5, bleaf false))


(* b) *)
let rec adepth = function
  | Empty -> 0
  | ANode (l, _, r) -> 1 + max (adepth l) (adepth r)
  | BNode (l, _, r) -> 
    let m = max (adepth l) (adepth r) in 
      if m > 0 then 1 + m else 0

let rec bdepth = function
  | Empty -> 0
  | BNode (l, _, r) -> 1 + max (bdepth l) (bdepth r)
  | ANode (l, _, r) -> 
    let m = max (bdepth l) (bdepth r) in 
      if m > 0 then 1 + m else 0

let b_2_1 = adepth test
let b_2_2 = bdepth test


(* c) *)
type result = {aNodes : int; bNodes : int}

let count abtree = 

  let rec count' counter = function
    | Empty -> counter
    | ANode (l, _, r) -> count' (count' {counter with aNodes = (counter.aNodes + 1)} l) r 
    | BNode (l, _, r) -> count' (count' {counter with bNodes = (counter.bNodes + 1)} l) r
    (* najprej je preštel v levem, nato še v desnem *)

  in
  count' {aNodes = 0; bNodes = 0} abtree

let c_2 = count test


(* d) *)
let is_typemirror ltree rtree = 

  let rec is_typemirror' = function (* kot argument sprejme (tree1, tree2) *)
    | (Empty, Empty) -> true
    
    | (ANode (l1, x1, r1), BNode (l2, x2, r2)) -> 
        (is_typemirror' (l1, l2) && is_typemirror' (r1, r2)) && (x1 = x2)

    | (BNode (l1, x1, r1), ANode (l2, x2, r2)) -> 
        (is_typemirror' (l1, l2) && is_typemirror' (r1, r2)) && (x1 = x2)
    
    | _ -> false (* karkoli druga od zgornjega je tudi slabo *)
  
  in
  is_typemirror' (ltree, rtree)

let d_2_1 = is_typemirror (ANode (Empty, 1, Empty)) (ANode (Empty, 1, Empty)) 
let d_2_2 = is_typemirror (ANode (Empty, 1, Empty)) (BNode (Empty, 1, Empty)) 


(* e) *)
let rec foldmap fa fb acc = function
  | Empty -> (acc, Empty)
  | ANode (l, x, r) -> let (result, element) = fa acc x in (* ne smemo z istim rezultatom v obe poddrevesi *)
                       let (left_res, left_tree) = foldmap fa fb result l in (* zato grem najprej po levem *)
                       let (right_res, right_tree) = foldmap fa fb left_res r in         (* nato po desnem *)
                       (right_res, ANode (left_tree, element, right_tree))      (* in vrnem željeno obliko *)
  
  | BNode (l, x, r) -> let (result, element) = fb acc x in 
                       let (left_res, left_tree) = foldmap fa fb result l in
                       let (right_res, right_tree) = foldmap fa fb left_res r in
                       (right_res, BNode (left_tree, element, right_tree))

(* tudi pri tej funkciji bi se lahko izognil definiranju kakšne nove spremenljivke a bi s tem izgubil na preglednosti, saj bi se le nalagale funkcije znotraj v argumentih  *)



(* assistent rekel da vrstni red argumentov lahko spremenimo *)
let e_2 = foldmap (fun acc x -> (acc+x, 0)) (fun acc b -> (acc-1, ())) 0 test 




