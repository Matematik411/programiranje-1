(* ========== Vaje 5: Urejanje  ========== *)


(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)
let randlist len max = 
  let rec randlist' acc = function
    | 0 -> acc
    | n -> randlist' ((Random.int max) :: acc) (n - 1)
  in
  randlist' [] len

let t = randlist 10 10

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

let tester our_sort len = 
  let test = randlist len len 
  in
  our_sort test = List.sort compare test

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)
let insert y xs = 
  let rec insert' acc y already = function

    | [] -> if already then List.rev acc
            else List.rev (y :: acc)

    | x :: rest when y < x -> if already then insert' (x :: acc) y already rest
                              else insert' (x :: y :: acc) y true rest

    | x :: rest -> insert' (x :: acc) y already rest

  in 
  insert' [] y false xs


let insert_snd y xs = () (*še drug način *)

(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)
let insert_sort list = 
  let rec insert_sort' acc = function
    | [] -> acc
    | x :: xs -> insert_sort' (insert x acc) xs
  in
  insert_sort' [] list


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)
let min_and_rest list = 
  let rec min' m = function
    | [] -> m
    | x :: xs -> min' (min x m) xs
  in
  let rec rest' acc a odstr = function
    | [] -> List.rev acc
    | x :: xs when a = x && not odstr -> rest' acc a true xs
    | x :: xs -> rest' (x :: acc) a odstr xs
  in
  match list with
  | [] -> None
  | x :: xs -> Some (min' x (x::xs), rest' [] (min' x (x::xs)) false (x :: xs))





(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh. 

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)
let selection_sort list = 

  let rec selection_sort' = function (* (list1, list2) *)
    | (list1, list2) ->   match min_and_rest list2 with
                            | None -> List.rev list1
                            | Some (m, xs) -> selection_sort' (m :: list1, xs)

  in selection_sort' ([], list)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)
let swap a i j = 
  let el = a.(i) in
  let () = a.(i) <- a.(j) in
  let () = a.(j) <- el in
  ()
(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 3
[*----------------------------------------------------------------------------*)

let index_min a lower upper = 
  let rec isci i mini = 
    if i > upper then mini
    else
    if a.(i) < a.(mini) then isci (i + 1) i
    else
    isci (i + 1) mini
  in
  isci lower lower

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Namig: Za testiranje uporabi funkciji [Array.of_list] in [Array.to_list]
 skupaj z [randlist].
[*----------------------------------------------------------------------------*)
let selection_sort_array a = 
  let d = Array.length a 
  in
  let rec selection_sort_array' n = 
    if n = (d - 1) then ()
    else  let i = index_min a n (d-1) in
          let () = swap a i n in
          selection_sort_array' (n+1)
  in
  selection_sort_array' 0


let tester_array our_sort len = 
  let test = randlist len len in
  let test_array = Array.of_list test in
  let () = our_sort test_array in
  Array.to_list test_array = List.sort compare test


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* bubble urejanje *)

let bubble_sort_array a =
  let d = Array.length a
  in
  let rec push_biggest = function
    | 0 -> ()
    | k ->  (let () = 
              (for i = 0 to (k-2) do 
              (if a.(i) > a.(i+1) then swap a i (i+1))
            done)
            in
            push_biggest (k-1))
  in
  push_biggest (d)

let aaa = [|1;3;4;2;3;12;9|]


let bubble_Pustoslemsek a = 
  let n = Array.length a
  in
  for i = (n-1) downto 0 do  (* tle si mel indekse narobe, da je vrglo index out of bounds *)
    let r = ref 0
    in 
    while !r < i do
      if a.(!r) > a.(!r + 1) then swap a !r (!r+1);
      r := !r + 1  (* ref se pa spreminja z := *)
    done
  done





