(* 1. naloga *)

(* a) *)
let option_sum a b =
  match (a,b) with
    | (Some a', Some b') -> Some (a' + b')
    | _ -> None

let a1_1 = option_sum (Some 3) (Some 5)
let a1_2 = option_sum (Some 3) (None)


(* b) *)
let twostep_map f g h a = 
  match f a with
    (b, c) -> (g b, h c)

let b1 = twostep_map (fun x -> (x,x)) ((+)1) ((-)2) 3

(* c) *)
let function_repeat f list =
  let rec build accum x = function
    | n when (n <= 0) -> accum
    | n -> build (x :: accum) x (n-1)
  in
  
  let rec function_repeat' f acc = function
    | [] -> List.rev acc
    | x :: xs -> function_repeat' f (build acc x (f x)) xs
  in
  function_repeat' f [] list

let c1 = function_repeat (fun x -> x+1) [1;2;3]
(* komentar:
  funkcija function_repeat' je repno rekurzivna, ker je njena rešitev le nov klic te funkcije same z drugimi argumenti, znotraj argumenta sicer kliče še drugo funkcijo, a je le ta prav tako repno rekurzivna (isti razlog), torej se klicanje ne nalaga, je le enostopenjsko, ko pa build doda dovoljkrat element x se klic zapre.  *)


(* d) *)
let rec iterate f cond a = 
  if cond a then a
  else iterate f cond (f a)

let d1 = iterate (fun x -> x+1) ((<)0) (-5)


(* ------------------------------------------------------------------- *)
(* 2. naloga *)

(* a) *)
type 'a improved_list =
  | End
  | Elt of 'a array * 'a improved_list

let test = Elt ([|1;2;20|], Elt ([|17;19;20;30|], Elt ([|100|], End)))

(* b) *)
let rec count = function
  | End -> 0
  | Elt (a, xs) -> (Array.length a) + count xs

let b2 = count test

(* c) *)
let rec nth n = function
  | Elt (a, xs) -> (match (n - (Array.length a)) with
                  | k when k >= 0 -> nth k xs
                  | k -> Some a.(n))
  | _ -> None

let c2_1 = nth 3 test
let c2_2 = nth 10 test

(* d) ---- linearna časovna zahtevnost ---  *) (* TA NE DELA *)
(* 
try 
  if el > sez.(0) then failwith "ni urejen"
  else
  let i = ref 0 in
  while true
    do if array.(i) > array.(i+1) then failwith "ni urejen"
    else i := !i + 1
  done
with "ni urejen" -> false
wit

 *)


(* let is_sorted impr_lst =
  let rec check el array =
    (* match elt with
      | None -> 
      | Some el ->  


    ( *)
    let a = Array.length array
    in
    if a = 0 then Some el
    
    else

    let b = ref (el < array.(0)) 
    in
    for i = 1 to (a - 1) do
      b := !b && (array.(i-1) < array.(i))
    done
    if b = true then (Some array.(i))
    else None
    
  
  in
  let rec is_sorted' prev = function
    | End -> true
    | Elt (a, xs) -> (match (check prev xs) with
                    | None -> false
                    | Some z -> is_sorted' z xs)
  in
  is_sorted' (-10) impr_lst *)


(* e) *)
(* ta funkcija vrne samo nov impoved_list torej mora indeks, ki ga vnesemo ustrezati *)
let rec update impr_lst n x =
  match impr_lst with
    | End -> End
    | Elt (a, xs) -> 
        (let nov = Array.copy a 
        in
        if n < 0 then Elt (nov, (update xs (-1) x))
        else
        match (n - (Array.length nov)) with
        | k when k >= 0 -> Elt (nov, (update xs k x))
        | k -> (let () = nov.(n) <- x in Elt (nov, (update xs (-1) x))))

let e2_1 = nth 5 (update test 5 (-3))
let e2_2 = nth 5 test











