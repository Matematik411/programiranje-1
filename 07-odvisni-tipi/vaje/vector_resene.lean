-- Vzamemo stvari iz predavanj
set_option autoImplicit false

inductive Naravno : Type where
  | nic : Naravno
  | naslednik : Naravno → Naravno
deriving Repr


def plus : Naravno → Naravno → Naravno :=
  fun m n =>
    match m with
    | Naravno.nic => n
    | Naravno.naslednik m' =>
        Naravno.naslednik (plus m' n)

-- Vektorji

inductive Vektor : Type → Naravno → Type where
  | prazen : {A : Type} → Vektor A Naravno.nic
  | sestavljen : {A : Type} → {n : Naravno} → A → Vektor A n → Vektor A (Naravno.naslednik n)
deriving Repr

#check (Vektor.sestavljen "a" (Vektor.sestavljen "b" (Vektor.prazen)))

def stakni_vektorja : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => ys
    | Vektor.sestavljen x xs' => Vektor.sestavljen x (stakni_vektorja xs' ys)


-- Sedaj lahko definiramo `lookup`, ki ne bo nikoli povzročil napake.
inductive Finite : Naravno -> Type where
  | fzero : {n : Naravno} -> Finite (Naravno.naslednik n)
  | fsucc : {n : Naravno} -> Finite n -> Finite (Naravno.naslednik n)


#check Finite (Naravno.nic)
#check Finite (Naravno.naslednik (Naravno.naslednik Naravno.nic))

-- def downcast: (a: Naravno) < (b : Naravno) -> Finite a -> Finite b

-- in to so vse mozne kombinacije za match !
def lookup {A : Type} {n : Naravno} : Vektor A n -> Finite n -> A :=
  fun xs i =>
    match xs, i with
    | Vektor.sestavljen x _, Finite.fzero => x
    | Vektor.sestavljen _ xs', Finite.fsucc (fi) => lookup xs' fi

def v : Vektor String (Naravno.naslednik (Naravno.naslednik Naravno.nic)) :=
Vektor.sestavljen "a" (Vektor.sestavljen "b" Vektor.prazen)

def idx0 : Finite (Naravno.naslednik (Naravno.naslednik Naravno.nic)) := Finite.fzero
def idx1 : Finite (Naravno.naslednik (Naravno.naslednik Naravno.nic)) := Finite.fsucc Finite.fzero
#eval lookup v idx0
#eval lookup v idx1

-- Včasih enakost tipov ni takoj očitna in jo moramo izpeljati
-- Dopolnite naslednjo definicijo, vse potrebne leme pa dokažite kar s taktiko `sorry`.

def plus_zero (n : Naravno) : (plus n Naravno.nic) = n := by
  induction n with
  | nic => rfl
  | naslednik d hd =>
    rw [plus]
    rw [hd]


def plus_add_suc (m n : Naravno) : (plus m (Naravno.naslednik n)) = (Naravno.naslednik (plus m n)) := by
  induction m with
  | nic =>
    rw [plus, plus]
  | naslednik d hd =>
    rw [plus, plus]
    rw [hd]

def plus_comm (m n : Naravno) : (plus m n) = (plus n m) := by
  induction m with
  | nic =>
    rw [plus_zero]
    rw [plus]
  | naslednik d hd =>
    rw [plus_add_suc]
    rw [← hd]
    rw [plus]




-- xs ys
-- xs @ ys : Vector A (n + m)
-- xs @ ys : Vector A (m + n)
def stakni_vektorja' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus n m) :=
fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
  match xs with
    | Vektor.prazen =>
      by
        rw [plus_zero]
        exact ys
    | Vektor.sestavljen x xs' =>
      by
        have aux := Vektor.sestavljen x (stakni_vektorja xs' ys)
        rw [plus_add_suc, plus_comm]
        exact aux


-- Uporabite samo definicijo `stakni_vektorja'` in taktike `rw` in `exact`.
def stakni_vektorja'' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    by
      have aux := stakni_vektorja' xs ys
      rw [plus_comm]
      exact aux


#print stakni_vektorja''
