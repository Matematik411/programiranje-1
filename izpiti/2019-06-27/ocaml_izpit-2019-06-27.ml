(* 1. *)

(* a), b) *)
type complex = {re: float; im: float}

let complex_add {re = r1; im = i1} {re = r2; im = i2} = {re = r1 +. r2; im = i1 +. i2}

let complex_conjugate {re = r1; im = i1} = {re = r1; im = -.i1}

let z = {re = 3.; im = 3.}
let b1 = complex_conjugate z

(* c) *)
let rec list_apply_either pred f g = function
  | [] -> []
  | x :: xs when pred x -> (f x) :: list_apply_either pred f g xs
  | x :: xs -> (g x) :: list_apply_either pred f g xs

let c1 = list_apply_either ((>) 3) (fun x -> x) (fun x -> -x) [1;2;3;4;5]

(* d) *)
let eval_poly list x0 =
  let rec power' x acc = function (* deluje za n >= 0 *)
    | 0 -> acc
    | n -> power' x (x * acc) (n-1)
  in
  let power x n = power' x 1 n
  in
  let rec eval_poly' value degree = function
    | [] -> value
    | x :: xs -> eval_poly' (x * (power x0 degree) + value) (degree + 1) xs
  in 
  eval_poly' 0 0 list 

let p = [3;-2;0;1]
let d1_1 = eval_poly p 0
let d1_2 = eval_poly p 3


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 2. *)

(* a) *)
type najemnik = string

type vrt = 
          | Prost
          | Obdelovan of najemnik
          | Oddan of najemnik * (vrt * vrt list)

let vrt_primer = Oddan 
                ("Kovalevskaya",
                (Prost, [Obdelovan "Galois"; Obdelovan "Lagrange"]))

(* b) *)
let obdelovalec_vrta = function
  | Prost | Oddan _ -> None
  | Obdelovan x -> Some x

let b2 = obdelovalec_vrta vrt_primer

(* c) *)
let rec globina_oddajanja = function
  | Prost | Obdelovan _ -> 0
  | Oddan (_, (a, [])) -> (1 + globina_oddajanja a)
  | Oddan (_, (a, x :: xs)) -> max (1 + globina_oddajanja a) (globina_oddajanja (Oddan (" ", (x,xs))))

let c2 = globina_oddajanja vrt_primer

(* d) *)
let rec v_uporabi = function
  | Prost -> false
  | Obdelovan _ -> true
  | Oddan (_, (a, [])) -> v_uporabi a
  | Oddan (_, (a, x :: xs)) -> (v_uporabi a) || (v_uporabi (Oddan (" ", (x,xs))))

let d2 = v_uporabi vrt_primer

(* e) *)
let rec vsi_najemniki = function
  | Prost -> []
  | Obdelovan os -> [os]
  | Oddan (oddaja, (prvi, ostali)) ->
      let vsi_od_seznama = 
        List.fold_left (fun a b -> a @ (vsi_najemniki b)) [] ostali
      in
      oddaja :: (vsi_najemniki prvi) @ (vsi_od_seznama)


let e2 = vsi_najemniki vrt_primer


(* f) *)
let rec vsi_obdelovalci = function
  | Prost -> []
  | Obdelovan os -> [os]
  | Oddan (_, (prvi, ostali)) ->
      let vsi_od_seznama = 
        List.fold_left (fun a b -> a @ (vsi_najemniki b)) [] ostali
      in
      (vsi_najemniki prvi) @ (vsi_od_seznama)

let f2 = vsi_obdelovalci vrt_primer



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* 3. *)

let zabojniki = [|1;3;4;7;10|]

let nacini_permutacije n =
  let st_zabojnikov = Array.length zabojniki
  in
  let koliko = Array.make (n + 1) 0
  in
  let izracunaj i = 
    if i = 0 then 1
    else
    let lokalno = ref 0
    in
    let rec j_ta_masa = function
      | 0 when ((i - zabojniki.(0)) >= 0) -> (lokalno := !lokalno + koliko.(i - zabojniki.(0)))
      | 0 -> ()
      | j when ((i - zabojniki.(j)) >= 0) -> (let () = (lokalno := !lokalno + koliko.(i - zabojniki.(j)))
                                            in
                                            j_ta_masa (j-1))
      | j -> j_ta_masa (j-1)
    in 
    let () = j_ta_masa (st_zabojnikov - 1)
    in
    !lokalno
  in
  let rec loop i =
    let () = koliko.(i) <- (izracunaj i)
    in
    if i < n then 
      loop (i + 1)
    else
      ()
  in
  let () = loop 0
  in
  koliko.(n)




let nacini n =
  let st_zabojnikov = Array.length zabojniki
  in
  let koliko = Array.make_matrix st_zabojnikov (n + 1) 0
  in
  let izracunaj i k = 
    if i = 0 then 1
    else
    let lokalno = ref 0
    in
    let rec j_ta_masa = function
      | 0 when ((i - zabojniki.(0)) >= 0) -> (lokalno := !lokalno + koliko.(0).(i - zabojniki.(0)))
      | 0 -> ()
      | j when ((i - zabojniki.(j)) >= 0) -> (let () = (lokalno := !lokalno + koliko.(j).(i - zabojniki.(j)))
                                            in
                                            j_ta_masa (j-1))
      | j -> j_ta_masa (j-1)
    in 
    let () = j_ta_masa k
    in
    !lokalno
  in
  let rec loop i =
    let rec vsi = function
      | 0 -> koliko.(0).(i) <- (izracunaj i 0)
      | k -> (let () = koliko.(k).(i) <- (izracunaj i k) in vsi (k-1))
    in
    let () = vsi (st_zabojnikov - 1)
    in
    if i < n then 
      loop (i + 1)
    else
      ()
  in
  let () = loop 0
  in
  koliko.(st_zabojnikov - 1).(n)




