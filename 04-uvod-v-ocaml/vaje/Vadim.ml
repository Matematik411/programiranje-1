(* --------------- Working with lists -----------------*)

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


(* ------------------ ARITHMETIC -------------------------------------- *)

(* 31. Determine whether a given integer number is prime. (medium) *)
(* I'm working with positive integers here! *)

let is_prime n = 
    let rec is_not_divisor n d = 
        d * d > n || (n mod d <> 0 && is_not_divisor n (d + 1))
    in
    n > 1 && is_not_divisor n 2

(* 32. Determine the greatest common divisor of two positive integer numbers. (medium) *)

let rec gcd m n =
    let o = m mod n
    in
    if o = 0 then n
    else gcd n o

(* 33. Determine whether two positive integer numbers are coprime. (easy) *)

let coprime m n = gcd m n = 1

(* 34. Calculate Euler's totient function φ(m). (medium) *)

let phi n = 
    let rec phi' n k acc =
        if n = 1 then 1
        else
        if n = k then acc
        else
        if coprime n k then phi' n (k + 1) (acc + 1)
        else
        phi' n (k + 1) acc
    in
    phi' n 1 0

(* 35. Determine the prime factors of a given positive integer. (medium) *)

let factors_list n = 
    if n = 1 then []
    else
    let rec factors_list' n k acc = 
        if n = 1 then List.rev acc
        else
        if n mod k = 0 then factors_list' (n / k) k (k :: acc)
        else
        factors_list' n (k + 1) acc
    in
    factors_list' n 2 []

(* 36. Determine the prime factors of a given positive integer (2). (medium) *)

let factors_pairs n = 
    let rec factors_pairs' acc current = function
        | [] -> [] (*doesn't happen *)
        | x :: [] -> (x, (current + 1)) :: acc
        | x :: y :: xs when x = y -> factors_pairs' acc (current + 1) (y :: xs)
        | x :: y :: xs -> factors_pairs' ((x, (current + 1)):: acc) 0 (y :: xs)
    in
    List.rev (factors_pairs' [] 0 (factors_list n))

(* 37. Calculate Euler's totient function φ(m) (improved). (medium) *)

let pow n k =
    let rec pow' n k acc = 
        if k = 0 then acc
        else pow' n (k - 1) (n * acc)
    in
    pow' n k 1


let phi_improved n = 
    let rec phi_improved' acc = function
        | [] -> acc
        | (x, k) :: xs -> phi_improved' (acc * (x - 1) * (pow x (k - 1))) xs
    in
    phi_improved' 1 (factors_pairs n)

