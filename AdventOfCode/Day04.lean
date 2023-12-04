import AdventOfCode.IO
import Lean.Data.HashSet

def testinput := "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

/-
def readNumber (s : String) : String × Nat :=
  (s.dropWhile Char.isDigit, (s.takeWhile Char.isDigit).toNat!)
-/

open Lean (HashSet)

def String.parseNumList (s : String) : List Nat :=
  ((s.trim.splitOn " ").map String.toNat?).filterMap id

def String.parseCards (s : String) : HashSet Nat :=
  HashSet.empty.insertMany $ (((s.splitOn ":").get! 1).parseNumList)

def parseLine (s : String) : HashSet Nat × List Nat :=
  let s := s.splitOn "|"
  ((s.get! 0).parseCards, (s.get! 1).parseNumList)

def countLine (win : HashSet Nat) (h : List Nat) : Nat :=
  match h with
  | [] => 0
  | h::hs => (if win.contains h then 1 else 0) + countLine win hs

def processLine (win : HashSet Nat) (h : List Nat) : Nat :=
  match countLine win h with
  | 0 => 0
  | n => 2^(n-1)

def uncurry (f : α → β → γ) (p : α × β) : γ :=
  f p.1 p.2

def List.processLines (s : List String) : List Nat :=
  ((s.map parseLine).map $ uncurry processLine)

def solve_one (s : List String) : Nat :=
  s.processLines.foldl Nat.add 0

-- SOLUTION 2

def List.countLines (s : List String) : List Nat :=
  (s.map parseLine).map $ uncurry countLine

-- adds c to the next n elements of counts
def List.addCounts (counts : List Nat) (n : Nat) (c : Nat) :=
  match counts, n with
  | [], _ => [] -- "Cards will never make you copy a card past the end of the table."
  | _, 0 => counts
  | (count::counts), Nat.succ n => (count+c)::(counts.addCounts n c)

def List.addScratchcards (scores : List Nat) (counts : List Nat) : Nat :=
  match scores, counts with
  | [], _ => 0
  | _, [] => panic! "no counts left?"
  | s::ss, n::ns => n + (ss.addScratchcards $ ns.addCounts s n)

def solve_two (s : List String) : Nat :=
  s.countLines.addScratchcards $ List.replicate s.length 1

def main : IO Unit := do
  let f ← IO.readInputForDay 4
  let f := f.trim.splitOn "\n"
  println! solve_one f
  println! solve_two f
