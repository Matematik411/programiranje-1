(* Vojne čarodejov se nadaljujejo. *)

(* Čarodeji, ki se borijo v vojnah so pripadniki teh treh ras.  *)
type race = Orc | Hobbit | Human


(* Uroki [spells] prihajajo iz treh šol [school] magije: firewall in blaze sta ognjena uroka [Fire],
   resurrect in cripple sta nekromantska [Necrotic], in renounce ter
   banish sta angelska [Angelic].

   Definiraj tipa, ki predstavljata različne uroke in šole magije.
*)


type school =
  | Fire 
  | Necrotic 
  | Angelic


type spell = 
  | Firewall 
  | Blaze 
  | Resurrect 
  | Cripple 
  | Renounce 
  | Banish

(* Veščine [skills], ki jih je čarodej osvojil, so seznam vseh urokov,
   ki jih lahko hitro izvede. Definiraj tip `skills'. *)

type skills = spell list

(* Čarodeja opišemo z imenom, številom življenskih točk [hp], sposobnost [ability]
   ki jo predstavimo s številom točk mane, raso [race] in veščino [skills].
   To shranimo kot zapisni tip (record). *)

type mana = Mana of int
type health = Hp of int

type wizard = {
  name : string; 
  hp : health;
  ability : mana;
  race : race;
  skills : skills
  }


(* Napiši funkcijo ki vsakemu uroku priredi primerno šolo magije. *)
let school_of_spell = function
  | Firewall | Blaze -> Fire
  | Resurrect | Cripple -> Necrotic
  | Banish | Renounce -> Angelic

(* Glede na tabelo napiši funkcijo, ki uroku priredi količino mane,
   ki jo čarodej potrebuje za izvršitev uroka:
  blaze : 420
  firewall : 35
  renounce : 17
  banish : 103
  resurrect : 178
  cripple : 250

   Namig: Lahko si pomagaš z regex-replace v Notepad++
 *)
let mana_of_spell = function
  | Blaze -> Mana 420
  | Firewall -> Mana 35
  | Renounce -> Mana 17
  | Banish -> Mana 103
  | Resurrect -> Mana 178
  | Cripple -> Mana 250

let int_of_mana (Mana x) = x

(* Ustvari nekaj primerov čarodejov, tako kot je prikazano na primeru Merlina.
   Ponovno si lahko pomagaš s regex-replace.*)
(*
name : "Frodo",      ability : 53,   hp : 1000,  skills : [Renounce],                      race : Hobbit
name : "Ajitam",     ability : 1337, hp : 7331,  skills : [Firewall; Resurrect; Firewall], race : Hobbit
name : "Mr Duck",    ability : 7,    hp : 90000, skills : [Cripple],                       race : Orc
name : "Kylo Ren",   ability : 589,  hp : 90,    skills : [Resurrect],                     race : Human
name : "Snoop Dogg", ability : 420,  hp : 4000,  skills : [Blaze],                         race : Orc
*)

(* let merlin = {name = "Merlin";   ability = 1832; hp = 9001; skills = [Renounce; Banish];  race = Human} *)
let merlin = {name = "Merlin"; ability = Mana 1832; hp = Hp 9001; skills = [Renounce; Banish]; race = Human}
let frodo = {name = "Frodo"; ability = Mana 53; hp = Hp 1000; skills = [Renounce]; race = Hobbit}
let ajitam = {name = "Ajitam"; ability = Mana 1337; hp = Hp 7331; skills = [Firewall; Resurrect; Firewall]; race = Hobbit}
let mrDuck = {name = "Mr Duck"; ability = Mana 7; hp = Hp 90000; skills = [Cripple]; race = Orc}
let kyloRen = {name = "Kylo Ren"; ability = Mana 589; hp = Hp 90; skills = [Resurrect]; race = Human}
let snoop_dogg = {name = "Snoop Dogg"; ability = Mana 420; hp = Hp 4000; skills = [Blaze]; race = Orc}


(* Napiši funkcijo, ki iz seznama čarodejev vrne čarodeja z največ mane. *)
let rec strongest_wizard (wizards : wizard list) =
  match wizards with
    | [] -> None (* do tega le če prazen seznam *)
    | x :: [] -> Some x
    | x :: y :: [] -> if x.ability >= y.ability then Some x else Some y
    | x :: y :: xs -> if x.ability >= y.ability then strongest_wizard (x :: xs) else strongest_wizard (y :: xs)

(* Posploši funkcijo strongest_wizard na funkcijo max_list, ki sprejme seznam
   in dodatno funkcijo dveh elementov max : 'a -> 'a -> 'a in vrne maksimalni element seznama
   glede na funkcijo max.
   RES SAMO KOPIRAŠ ZGORNJO IN ZAMENJAŠ FUNKCIJO
*)



(* Rase imajo različno občutljivost [vulnerability] na določene šole magije.
   Napiši tip s katerim lahko izraziš kdaj ima rasa visoko [High], navadno [Normal]
   ali pa nizko [Low] občutljivost na urok. *)

type vulnerability = 
  | High
  | Normal
  | Low

(* Napiši funkcijo, ki glede na šolo uroka in raso izračuna občutljivost.

   Low za:     orcs:necrotic, hobbits:fire, humans:angelic,
   High za:    hobbit:necrotic, human:fire, orc:angelic
   Sicer vrne Normal
*)

let effectiveness (school : school) (race : race) : vulnerability =
  let low = function
    | Orc -> Necrotic
    | Hobbit -> Fire
    | Human -> Angelic
  in
  let high = function
    | Orc -> Angelic
    | Hobbit -> Necrotic
    | Human -> Fire
  in
  if school = low race then Low
  else if school = high race then High
  else Normal


(* Zapiši funkcijo, ki za čarodeja izračuna njegovo občutljivost na podani urok. *)
let vulnerable spell wizard = effectiveness (school_of_spell spell) wizard.race


(* Občutljivost se v boju izrazi kot koeficient škode, ki jo utrpi čarodej, če ga urok zadane.
   Zapiši funkcijo, ki glede na občutljivost vrne primeren koeficient, tako da čarodej z nizko
   občutljivostjo utrpi le pol škode, čarodej z visoko občutljivostjo pa dvakratnik.*)

let koefficient spell wizard = 
  match vulnerable spell wizard with
    | Normal -> 1.
    | Low -> 0.5
    | High -> 2.

(* Vsak urok naredi toliko škode, kot je potrebnih točk mane za izvršitev uroka.
   Napiši funkcijo, ki glede na urok in čarodeja izračuna koliko škode utrpi,
   če ga urok zadane.

   Namig: za pretvarjanje med int in float se uporabljata funkciji float_of_int in
   int_of_float.
*)

let damage_taken spell wizard =
  int_of_float ((float_of_int (int_of_mana (mana_of_spell spell))) *. (koefficient spell wizard))

(* Zapiši funkcijo, ki vrne novo stanje čarodeja (z znižanimi življenskimi točkami [hp]),
   po tem, ko ga je zadel izbrani urok.
   (Novo stanje čarodeja je prav tako tipa wizard)
*)

let after_hit spell wizard =
  let (Hp x) = wizard.hp 
  in
  {wizard with hp = Hp (x - (damage_taken spell wizard))}

(* Napiši funkcijo, ki za danega čarovnika izvršuje uroke, dokler ne izvede vseh urokov
   na seznamu, ali pa mu zmanjka točk mane.
   TOREJ izvede prvi spell, če ga lahko ga da NA KONEC skills, zato da se spelli ciklajo, če ga ne more, pa ga vrže ven.
   Vrne seznam spellov *)

let cast_spells wizard : spell list = 
  let spells = wizard.skills
  in
  let rec cast_spells' acc remaining = function
    | [] -> List.rev acc
    | x :: xs when remaining >= (int_of_mana (mana_of_spell x)) -> cast_spells' (x :: acc) (remaining - (int_of_mana (mana_of_spell x))) (List.rev (x ::(List.rev xs)))
    | x :: xs -> cast_spells' acc remaining xs

  in
  cast_spells' [] (int_of_mana (wizard.ability)) spells


(* Napiši funkcijo, ki simulira spopad dveh čarodejev. V primeru, ko napadalec ne more izvršiti
   nobenega uroka, napadalec izgubi. V nasprotnem primeru uporabi vse uroke, ki jih lahko.
   Če branilcu zmanjka življenskih točk, izgubi, sicer pa se vlogi napadalca in branilca zamenjata.
   TOREJ to je bitka !DO SMRTI!, če po napadalčevem krogu branilec še živi, potem napada zdaj on!
*)
let rec duel (attacker : wizard) (defender : wizard) =
  let casting attacker defender = 
    let rec casting' attacker defender = function
      | [] -> attacker, defender
      | x :: xs -> casting' attacker (after_hit x defender) xs
    in
    casting' attacker defender (cast_spells attacker)
  in
  let fst, snd = (casting attacker defender)
  in
  if snd.hp > Hp 0 then duel snd fst
  else fst

