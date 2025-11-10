-- Strukture:

-- (A x B) ^ C <=> A ^ C x B ^ C
def eksponent (A B C : Type) (f : C → Prod A B) : Prod (C → A) (C → B) :=
  ⟨
    fun (x : C) => (f x).1,
    fun (x : C) => (f x).2
  ⟩
def eksponent_prop (A B C : Prop) (f : C → A ∧ B) : (C → A) ∧ (C → B) :=
  ⟨
    fun (x : C) => (f x).1,
    fun (x : C) => (f x).2
  ⟩
def eksponent_prop_s_taktikami (A B C : Prop) (f : C → A ∧ B) : (C → A) ∧ (C → B) :=
  by
    constructor
    · intro H
      have Hatf : A ∧ B := f H
      exact Hatf.left
    · intro H
      apply And.right
      apply f
      exact H



-- ------------------------------
-- Logika

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  ⟨
    fun ab => ⟨ ab.2, ab.1 ⟩,
    fun ba => ⟨ ba.2, ba.1 ⟩
  ⟩

theorem eq1' {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  Iff.intro
    (fun h => And.intro h.right h.left)
    (fun g => And.intro g.2 g.1)

theorem eq1'' {A B : Prop} : (A ∧ B) ↔ (B ∧ A) := by
  constructor
  · intro h
    constructor
    · exact h.right
    · exact h.left
  · intro h
    apply And.intro
    · apply And.right
      exact h
    · exact h.left

theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) := by
  constructor
  · intro h
    cases h with
    | inl ha =>
      apply Or.inr
      exact ha
    | inr hb =>
      apply Or.inl
      exact hb
  . intro h
    cases h with
    | inl hb => exact Or.inr hb
    | inr ha => exact Or.inl ha

#check And.left

theorem eq2' {A B : Prop} : (A ∨ B) ↔ (B ∨ A) := by
  constructor
  · intro h
    apply Or.elim h
    · intro f
      apply Or.inr
      exact f
    · exact Or.inl
  · intro h
    apply Or.elim h
    · exact Or.inr
    · exact Or.inl

theorem eq2'' {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  Iff.intro
    (fun AorB => AorB.elim Or.inr Or.inl)
    (fun BorA => BorA.elim Or.inr Or.inl)

theorem eq2''' {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
⟨
  fun h => h.elim Or.inr Or.inl,
  fun h => h.elim Or.inr Or.inl,
⟩


theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
⟨
  fun h => ⟨ h.2.1, ⟨ h.1, h.2.2 ⟩ ⟩,
  fun h => ⟨ h.2.1, ⟨ h.1, h.2.2 ⟩ ⟩
⟩

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) := by
  apply Iff.intro
  · intro h1
    cases h1 with
    | inl ha =>
      apply Or.inr
      exact Or.inl ha
    | inr hbc =>
      cases hbc with
      | inl hb => exact Or.inl hb
      | inr hc => exact Or.inr (Or.inr hc)
  · intro h2
    cases h2 with
    | inl hb => exact Or.inr (Or.inl hb)
    | inr hac =>
      cases hac with
      | inl ha => exact Or.inl ha
      | inr hc => exact Or.inr (Or.inr hc)



theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  Iff.intro
    (fun h => match h with
      | ⟨ ha, Or.inl hb ⟩ => Or.inl (And.intro ha hb)
      | ⟨ ha, Or.inr hc ⟩ => Or.inr ⟨ ha, hc ⟩ )
    (fun g => match g with
      | Or.inl ⟨ ha, hb ⟩ => And.intro ha (Or.inl hb)
      | Or.inr ⟨ ha, hc ⟩ => ⟨ ha, Or.inr hc ⟩ )

theorem eq5' {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) := by
  constructor
  · intro h1
    obtain ⟨ ha, hbc ⟩ := h1
    cases hbc with
    | inl hb =>
      apply Or.inl
      apply And.intro
      · exact ha
      · exact hb
    | inr hc => exact Or.inr ⟨ ha, hc ⟩
  · intro h2
    cases h2 with
    | inl hab =>
      apply And.intro
      · exact hab.1
      · exact Or.inl (hab.2)
    | inr hac => exact ⟨ hac.1, Or.inr hac.2 ⟩

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) := by
  apply Iff.intro
  · intro h1
    apply And.intro
    · intro hb
      apply h1
      left
      assumption
    · intro hc
      exact h1 (Or.inr hc)
  · intro h2
    intro hbc
    obtain ⟨ ba, ca ⟩ := h2
    cases hbc with
    | inl hb => exact ba hb
    | inr hc => exact ca hc


theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) := by
  apply Iff.intro
  · intro h1
    apply And.intro
    · intro hc
      exact (h1 hc).left
    · intro hc
      exact (h1 hc).2
  · intro h2
    intro hc
    apply And.intro
    · exact h2.left hc
    · exact h2.right hc


-- ------------------------------
-- Enakosti naravnih števil (z uporabo `calc`)

#check Nat.mul_pow

theorem kvadrat_dvoclenika {a b : Nat} : (a + b)^2 = a^2 + 2 * a * b + b^2 :=
  by
    calc
      (a + b)^2
      _ = (a + b) * (a + b) := by rw [Nat.pow_two]
      _ = (a + b) * a + (a + b) * b := by rw [Nat.mul_add]
      _ = a * a + b * a + (a * b + b * b) := by rw [Nat.add_mul, Nat.add_mul]
      _ = ((a * a + b * a) + (a * b + b * b)) := by rfl
      _ = a * a + (b * a + (a * b + b * b)) := by rw [Nat.add_assoc]
      _ = a * a + ((b * a + a * b) + b * b) := by rw [← Nat.add_assoc (b*a) (a*b) (b*b)] -- lahko tudi rw [Nat.add_assoc]
      _ = a * a + ((a * b + a * b) + b * b) := by rw [Nat.mul_comm b a]
      _ = a * a + (2 * (a * b) + b * b) := by rw [Nat.two_mul]
      _ = a * a + 2 * (a * b) + b * b := by rw [Nat.add_assoc]
      _ = a^2 + 2 * (a * b) + b^2 := by rw [← Nat.pow_two, ← Nat.pow_two]
      _ = a^2 + 2 * a * b + b^2 := by rw [Nat.mul_assoc]


theorem vsota_eksponent_produkta {a b c d : Nat} : (a * b)^(c + d) = (a^c)*(a^d)*(b^c)*(b^d) :=
  by
    calc
      (a * b)^(c + d)
      _ = a^(c + d) * b^(c + d) := by rw [Nat.mul_pow]
      _ = (a^c * a^d) * (b^c * b^d) := by rw [Nat.pow_add, Nat.pow_add]
      _ = (a^c * a^d) * b^c * b^d := by rw [← Nat.mul_assoc]
      _ = a^c * a^d * b^c * b^d := by rfl
