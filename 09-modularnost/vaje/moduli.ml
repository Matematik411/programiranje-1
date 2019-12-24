(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig 
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one : t
  val add : t -> t -> t
  val substract : t -> t -> t
  val multiply : t -> t -> t
  val to_int : t -> int 
  val of_int : int -> t
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct
  type t = int
  
  let eq x y = (x = y)
  let zero = 0
  let one = 1
  let add x y = x + y
  let substract x y = if x > y then x - y else 0
  let multiply x y = x * y
  let to_int x = x 
  let of_int x = x
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = 
    | Z
    | S of t 
  let eq x y = (x = y)
  let zero = Z 
  let one = S Z
  let rec add x = function
    | Z -> x
    | S a -> S (add x a)
  let rec substract x y = 
    match (x, y) with
    | (Z, _) -> Z
    | (S a, Z) -> S a
    | (S a, S b) -> substract a b
  let rec multiply x = function
    | Z -> Z
    | S a -> add x (multiply x a)
  let rec to_int = function
    | Z -> 0
    | S a -> 1 + (to_int a)
  let rec of_int = function
    | 0 -> Z
    | x -> S (of_int (x-1))
end
let sedem = Nat_peano.of_int 7
let osem = Nat_peano.of_int 8
let test = Nat_peano.multiply sedem osem
let ali_je_56 = Nat_peano.to_int test

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot 

 # f (module M_implementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) = 
  let nr = 99
  in
  let rec add' = function
    | 0 -> Nat.zero
    | x -> Nat.add 
        (add' 
            (Nat.to_int 
                (Nat.substract 
                    (Nat.of_int x) 
                    Nat.one))
        )
        (Nat.of_int x)
  in
  add' nr |> Nat.to_int

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t
  val one : t
  val im_one : t
  val negate : t -> t
  val conjugate : t -> t
  val add : t -> t -> t
  val multiply : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x = y
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let im_one = {re = 0.; im = 1.}
  let negate {re = a; im = b} = {re = -.a; im = -.b}
  let conjugate {re = a; im = b} = {re = a; im = -.b}
  let add z w = {re = (z.re +. w.re); im = (z.im +. w.im)}
  let multiply z w = {re = (z.re *. w.re) -. (z.im *. w.im); im = (z.im +. w.re) +. (z.re +. w.im)}
  
  

end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}


  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad deg = (deg /. 180.) *. pi
  let deg rad = (rad /. pi) *. 180.

  let eq x y =
    x.magn = y.magn &&
      (x.magn = 0. ||
      (mod_float x.arg 2. *. pi =
       mod_float y.arg 2. *. pi))

  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let im_one = {magn = 1.; arg = pi}

  let negate {magn; arg} = {magn; arg = mod_float (arg +. pi) 2.*.pi}
  let conjugate {magn; arg} = {magn; arg = mod_float (-.arg) 2.*.pi}

  let multiply z w = {magn = (z.magn *. w.magn); arg = mod_float (z.arg +. w.arg) 2.*.pi} 
  (* SHOUT-OUT to the MVP who actually wrote all of this down! #Respect !!! *)
  (* All of this for addition... *)
  let re {magn; arg} = magn *. cos (rad arg)
  let im {magn; arg} = magn *. sin (rad arg)

  let arg re im =
    let rad =
      if re > 0. then atan (im /. re)
      else if re < 0. && im >= 0. then atan (im /. re) +. pi
      else if re < 0. && im < 0. then  atan (im /. re) -. pi
      else if re = 0. && im > 0. then pi /. 2.
      else if re = 0. && im < 0. then -.(pi /. 2.)
      else 0.
    in deg rad

  let magn re im = sqrt (re *. re +. im *. im)

  let add x y =
    let square x = x *. x in
    let magn = sqrt (square x.magn +. square y.magn +. 2. *. x.magn *. y.magn *. cos (y.arg -. x.arg))
    and arg = x.arg +.
                atan2 (y.magn *. sin (y.arg -. x.arg))
                      (x.magn +. y.magn *. cos (y.arg -. x.arg)) in
    {magn; arg}

  let add' x y =
    let z_re, z_im = re x +. re y, im x +. im y in
    let arg = arg z_re z_im
    and magn = magn z_re z_im
    in {arg; magn}
    (* THANK YOU, kind sir! *)
end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)
module type DICT = sig
  type ('key, 'value) t
  
  val empty : ('key, 'value) t
  val get : 'key -> ('key, 'value) t -> 'value option
  val insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val print : (string, int) t -> unit
end

module Dict_tree : DICT = struct
  type ('key, 'value) t = 
    | Empty
    | Node of ('key, 'value) t * ('key *  'value) * ('key, 'value) t

  let empty = Empty

  let rec get key = function
    | Empty -> None
    | Node (l, (k, v), r) when key < k -> get key l
    | Node (l, (k, v), r) when key > k -> get key r
    | Node (l, (k, v), r)-> Some v
  
  let rec print = function
    | Empty -> ()
    | Node (l, (k, v), r) -> 
        (print l);
        print_string (k ^ " : " ^ (string_of_int v) ^ "\n");
        (print r) 
  
  let rec insert key value = function
    | Empty -> Node (Empty, (key, value), Empty)
    | Node (l, (k, v), r) when key < k -> Node (insert key value l, (k, v), r)
    | Node (l, (k, v), r) when key > k -> Node (l, (k, v), insert key value r)
    | Node (l, (k, v), r) -> Node (l, (key, value), r)

end
(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
module D = Dict_tree

let count list =
  let rec create_object acc = function
    | [] -> acc
    | x :: xs ->
      match D.get x acc with
        | None -> create_object (D.insert x 1 acc) xs
        | Some a -> create_object (D.insert x (a + 1) acc) xs
  in
  list |> create_object D.empty |> D.print
