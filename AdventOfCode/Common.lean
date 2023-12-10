
def String.parseNatList (s : String) : List Nat :=
  ((s.trim.splitOn " ").map String.toNat?).filterMap id

def String.parseIntList (s : String) : List Int :=
  ((s.trim.splitOn " ").map String.toInt?).filterMap id

def List.partitionWhile (l : List α) (f : α → Bool) : List α × List α :=
  let rest := l.dropWhile f
  (l.take (l.length - rest.length), rest)

def uncurry (f : α → β → γ) (p : α × β) : γ :=
  f p.1 p.2

def List.indexOf? [BEq α] (l : List α) (a : α) := go l 0 where
  go : List α → Nat → Option Nat
  | [], _ => Option.none
  | l::ls, n => if a == l then n else go ls $ n+1

def List.indexOf! [BEq α] (l : List α) (a : α) : Nat :=
  match List.indexOf? l a with
  | Option.none => panic! s!"Element not found."
  | Option.some i => i
