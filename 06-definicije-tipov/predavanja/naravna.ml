type naravno =
  | Nic
  | Naslednik of naravno

let nic = Nic
let ena = Naslednik nic
let dva = Naslednik ena
let tri = Naslednik dva
let stiri = Naslednik tri

let rec sestej m = function
  | Nic -> m
  | Naslednik n -> Naslednik (sestej m n)

let rec zmnozi m = function
  | Nic -> Nic
  | Naslednik n -> sestej (zmnozi m n) m

let rec potenciraj m = function
  | Nic -> Naslednik Nic
  | Naslednik n -> zmnozi (potenciraj m n) m

let rec zlozi f acc = function
  | Nic -> acc
  | Naslednik n -> f (zlozi f acc n)

let pretvori_v_int n = zlozi succ 0 n

let sestej' m n = zlozi (fun vsota -> Naslednik vsota) m n
let zmnozi' m n = zlozi (fun produkt -> sestej' produkt m) Nic n
let potenciraj' m n = zlozi (fun potenca -> zmnozi' potenca m) ena n

let naravno_v_int n = 
  let rec naravno_v_int' acc = function
    | Nic -> acc
    | Naslednik n' -> naravno_v_int' (acc + 1) (n')
  in
  naravno_v_int' 0 n

let int_v_naravno n = 
  let rec int_v_naravno' acc = function
    | 0 -> acc
    | n' -> int_v_naravno' (Naslednik acc) (n' - 1)
  in
  int_v_naravno' Nic n