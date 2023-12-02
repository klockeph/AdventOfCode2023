import AdventOfCode.IO

def lines (s : String) : List String :=
  s.splitOn "\n"

def getAsNat (c : Char) : Option Nat :=
  if c.isDigit then Option.some (c.toNat - 0x30) else Option.none

def numberWords : List String := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

def getNumberWord (s : String) (rev : Bool) : Option Nat :=
  match numberWords.dropWhile (λw => if !rev then !s.startsWith w else !s.startsWith (String.mk w.toList.reverse)) with
  | [] => Option.none
  | ws => Option.some $ 10 - ws.length

-- #eval getNumberWord "five"
-- #eval getNumberWord "nine5"
-- #eval getNumberWord "nines512ac"

-- ======= [First Problem] =======
def calibValue1' (s : List Char) (first : Nat) (last : Nat) : Nat :=
  match s with
  | [] => first*10 + last
  | c :: cs => match getAsNat c with
    | Option.none => calibValue1' cs first last
    | Option.some d => calibValue1' cs first d

def calibValue1 (s : List Char) : Nat :=
  match s with
  | [] => 0
  | c :: cs => match getAsNat c with
    | Option.none => calibValue1 cs
    | Option.some d => calibValue1' cs d d
-- ====== [/First Problem] =======

def findFirstNumOrDigit (s : List Char) (rev : Bool): Option Nat :=
  match s with
  | [] => Option.none
  | s@(c::cs) => ((getNumberWord (String.mk s) rev).orElse (λ_ => getAsNat c)).orElse (λ_ => findFirstNumOrDigit cs rev)

def calib (s : List Char) : Option Nat := do
  let x ← findFirstNumOrDigit s false
  let y ← findFirstNumOrDigit s.reverse true
  return x*10 + y

def reduceOption (a : List (Option α)) : List α := a.filterMap id

-- TODO: could solve this easier(?) by finding first & last digit.

def solve_first (f : String) : Nat :=
  let calibs := (lines f).map (λs => calibValue1 (s.toList))
  calibs.foldl Nat.add 0

def solve_second (f : String) : Nat :=
  let calibs := reduceOption $ (lines f).map (λs => calib (s.toList))
  calibs.foldl Nat.add 0

def main : IO Unit := do
  let x ← IO.readInputForDay 1
  IO.println $ solve_first x
  IO.println $ solve_second x
