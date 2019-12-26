(* 1 *)

(* a) *)
let odstej_trojici (a, b, c) (d, e, f) = (a-d, b-e, c-f)

(* b) *)
let max_rezultat_do_n f n = 
  List.fold_left max (f n) (List.init n f)

let b1_1 = max_rezultat_do_n (fun x -> -x) 10

(* c) *)
let pocisti_seznam list = 
  let rec pocisti_seznam' acc = function
    | [] -> List.rev acc
    | (Some x) :: xs -> pocisti_seznam' (x :: acc) xs
    | _ :: xs -> pocisti_seznam' acc xs
  in
  pocisti_seznam' [] list

(* d) *)
let preveri_urejenost list = 
  let rec preveri_urejenenost' even_acc odd_acc = function
    | [] -> true
    | x :: xs when (x mod 2 = 0) -> (match even_acc with
                                    | None -> preveri_urejenenost' (Some x) odd_acc xs
                                    | Some y when x > y -> preveri_urejenenost' (Some x) odd_acc xs
                                    | Some y -> false)
    | x :: xs -> (match odd_acc with
                | None -> preveri_urejenenost' even_acc (Some x) xs
                | Some y when x < y -> preveri_urejenenost' even_acc (Some x) xs
                | Some y -> false)
  in preveri_urejenenost' None None list
    
let d1_1 = preveri_urejenost [5;2;4;1;6]
let d1_2 = preveri_urejenost [3;2;4;5;6]

