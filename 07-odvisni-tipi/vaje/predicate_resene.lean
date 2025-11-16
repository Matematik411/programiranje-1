
variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

-- Izjave napišite na list papirja, nato pa jih dokažite v datoteki.

example : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro h
      intro x
      intro hp
      apply h
      -- exact ⟨ x, hp ⟩
      apply Exists.intro x
      exact hp
    · intro h1 h2
      obtain ⟨x, hpx⟩ := h2
      apply (h1 x)
      exact hpx




example : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    · intro rToForAllpx hx hr
      have forAllpx := rToForAllpx hr
      specialize forAllpx hx
      exact forAllpx
    · intro forAllrToPx hr hx
      specialize forAllrToPx hx
      exact forAllrToPx hr



example : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) :=
  by
    apply Iff.intro
    · intro rAndExistsPx
      obtain ⟨ hr, hExistsPx ⟩ := rAndExistsPx
      obtain ⟨ hx, hpx ⟩ := hExistsPx
      exact ⟨ hx, ⟨ hr, hpx ⟩ ⟩
    · intro existsrAndPx
      obtain ⟨ hx, rAndPx ⟩ := existsrAndPx
      exact ⟨ rAndPx.left, ⟨ hx, rAndPx.right ⟩ ⟩

example : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  by
    intro rOrForallPx hx
    cases rOrForallPx with
    | inl hr => exact Or.inl hr
    | inr forAllPx =>
      exact Or.inr (forAllPx hx)

-- Tu pa nam bo v pomoč klasična logika
-- namig: `Classical.byContradiction` in `Classical.em` sta lahko v pomoč
open Classical
#check Classical.em

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
  by
    apply Iff.intro
    · intro notForallpx
      apply Classical.byContradiction
      intro notExistsNotPx
      apply notForallpx
      intro hx
      apply Classical.byContradiction
      intro notPx
      apply notExistsNotPx
      exact ⟨ hx, notPx ⟩
    · intro existsNotPx forallPx
      obtain ⟨ hx, notPx ⟩ := existsNotPx
      apply notPx
      exact forallPx hx


example : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    · intro rOrForallpx hx
      cases rOrForallpx with
      | inl hr =>
        exact Or.inl hr
      | inr forallPx =>
        exact Or.inr (forallPx hx)
    · intro forallrOrPx
      have emR := Classical.em r
      cases emR with
      | inl hr => exact Or.inl hr
      | inr nr =>
        apply Or.inr
        intro hx
        specialize forallrOrPx hx
        cases forallrOrPx with
        | inl hr => contradiction
        | inr hpx => exact hpx
