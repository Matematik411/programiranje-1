(* -------- 1 -------- *)
let vsota_seznama =
  let rec vsota_seznama' acc = function
    | [] -> acc
    | x :: xs -> vsota_seznama' (x + acc) xs
  in
  vsota_seznama' 0
(* -------- 2 -------- *)
let rec preveri_urejenost = function
  | [] | _ :: [] -> true
  | x :: y :: xs when x < y -> preveri_urejenost (y :: xs)
  | x :: y :: xs -> false
(* -------- 3 -------- *)
let dodaj_v_urejen list n = 
  let rec dodaj_v_urejen' acc dodano = function
    | [] when dodano -> List.rev acc
    | [] -> List.rev (n :: acc)
    | x :: xs when dodano -> dodaj_v_urejen' (x :: acc) dodano xs
    | x :: xs when n < x -> dodaj_v_urejen' (x :: n :: acc) true xs
    | x :: xs -> dodaj_v_urejen' (x :: acc) dodano xs
  in
  dodaj_v_urejen' [] false list

let uredi_seznam = 
  let rec uredi_seznam' acc = function
    | [] -> acc
    | x :: xs -> uredi_seznam' (dodaj_v_urejen acc x) xs
  in
  uredi_seznam' []
(* -------- 4 -------- *)
let dodaj_posebna list cmp a = 
  let rec dodaj_posebna' acc dodano = function
    | [] when dodano -> List.rev acc
    | [] -> List.rev (a :: acc)
    | x :: xs when dodano -> dodaj_posebna' (x :: acc) dodano xs
    | x :: xs when cmp a x -> dodaj_posebna' (x :: a :: acc) true xs
    | x :: xs -> dodaj_posebna' (x :: acc) dodano xs
  in
  dodaj_posebna' [] false list

let uredi_posebna cmp = 
  let rec uredi_posebna' acc = function
    | [] -> acc
    | x :: xs -> uredi_posebna' (dodaj_posebna acc cmp x) xs
  in
  uredi_posebna' []
(* -------- 5 -------- *)
type priority = 
  | Top
  | Group of int

type status = 
  | Staff
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)
let prej prvi drugi = 
  match prvi.status, drugi.status with
    | Staff, _ -> true
    | _, Staff -> false
    | Passenger x, Passenger y -> 
      (
        match x, y with 
          | Top, _ -> true
          | _, Top -> false
          | Group a, Group b when a > b -> true
          | Group a, Group b -> false 
      )
let uredi_seznam_potnikov = 
  uredi_posebna prej
(* -------- 7 -------- *)
let test = 
  let rec test' acc = function
    | [] -> acc
    | x :: xs -> [x]

  in
  test' []

let naredi_bloke = 
  let rec naredi_bloke' (acc : flyer list list) = function
  | [] -> acc
  | {status = s1; name = n1} :: xs -> 
  (
    let rec kam_spada (accblokov : flyer list list) = function 
      | [] -> naredi_bloke' ([{status = s1; name = n1}] :: accblokov) xs
      | ({status = s2; name = n2} :: ys) :: rest when s1 = s2 -> naredi_bloke' ((({status = s1; name = n1} :: {status = s2; name = n2} :: ys) :: rest) @ accblokov) xs 
      | ({status = s2; name = n2} :: ys) :: rest -> kam_spada (({status = s2; name = n2} :: ys) :: accblokov) rest
      | _ -> failwith "se ne zgodi"
    in
    kam_spada [] acc
  )
  in
  naredi_bloke' []

let primerjaj_bloke (a :: _) (b :: _) =
  prej a b

let uredi_po_blokih list = 
  let bloki = naredi_bloke list 
  in
  uredi_posebna (primerjaj_bloke) bloki

(* let boarding_blocks_OG lst =
  let seq = boarding_sequence lst in
  let rec aux blocks block stat = function
    | [] -> block :: blocks
    | f :: fs ->
      if stat = f.status then
         aux blocks (f::block) stat fs
      else
         aux (block :: blocks) [f] f.status fs
  in
  match seq with
  | [] -> [[]]
  | f :: fs ->
    let blocks = aux [] [f] f.status fs in
    List.rev blocks
   *)
