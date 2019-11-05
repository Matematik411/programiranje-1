(* 7. Flatten a nested list structure. (medium) *)

type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten list = 
    let rec flatten' acc = function
        | [] -> acc
        | One x :: xs -> flatten' (x :: acc) xs
        | Many x :: xs -> flatten' (flatten' acc x) xs
    in
    List.rev (flatten' [] list)

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let eliminate list = 
    let rec eliminate' acc = function
        | [] | _ :: [] -> failwith "prekratek seznam"
        | x :: y :: [] when x = y -> x :: acc
        | x :: y :: [] -> x :: y ::acc
        | x :: y :: xs when x = y -> eliminate' (acc) (y :: xs)
        | x :: y :: xs -> eliminate' (x :: acc) (y :: xs)
    in
    List.rev (eliminate' [] list)

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack list = 
    let rec pack' acc current = function
        | [] -> acc
        | x :: [] -> List.rev ((x :: current) :: acc)
        | x :: y :: xs when x = y -> pack' acc (x :: current) (y :: xs)
        | x :: y :: xs -> pack' ((x :: current) :: acc) [] (y :: xs)
    in 
    pack' [] [] list

(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate list n = 
    let rec replicate' n acc k = function 
        | [] -> List.rev acc
        | x :: xs when k = 0 -> replicate' n acc n xs
        | x :: xs -> replicate' n (x :: acc) (k - 1) (x :: xs)
    in 
    replicate' n [] n list

(* 16. Drop every N'th element from a list. (medium) *)

let drop list n = 
    let rec drop' n acc k = function
        | [] -> List.rev acc
        | x :: xs when k = n -> drop' n acc 1 xs
        | x :: xs -> drop' n (x :: acc) (k + 1) xs
    in
    drop' n [] 1 list

(* 18. Extract a slice from a list. (medium) *)

let slice list i k = 
    let rec slice' i k acc j = function
        | [] -> List.rev acc
        | x :: xs when i <= j && j <= k -> slice' i k (x :: acc) (j + 1) xs
        | x :: xs -> slice' i k acc (j + 1) xs
    in
    slice' i k [] 0 list