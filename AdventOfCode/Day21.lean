import AdventOfCode.IO
import Lean.Data.HashSet

open Lean (HashSet)

def testinput := "
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
".trim

abbrev Tiles := List (List Char)
abbrev Pos := (Nat × Nat)

def Tiles.at (t : Tiles) (p : Pos) : Char :=
  t.get! p.2
  |>.get! p.1

def Pos.isValid (t : Tiles) (p : Pos) : Bool :=
  p.1 ≥ 0 ∧ p.1 < t.length ∧ p.2 ≥ 0 ∧ p.2 < t.length

def Pos.getNeighbors (p : Pos) (t : Tiles) : List Pos :=
  [(p.1 - 1, p.2), (p.1 + 1, p.2), (p.1, p.2 + 1), (p.1, p.2 - 1)]
  |>.filter λp => Pos.isValid t p ∧ t.at p ≠ '#'

def Tiles.findStart (t : Tiles) : Pos := go t 0 where
  go : Tiles → Nat → Pos
    | [], _ => panic! "Start not found!"
    | (l::ls), x => match go' l x 0 with
      | none => go ls $ x+1
      | some p => p
  go' : List Char → Nat → Nat → Option Pos
    | [], _, _ => none
    | (c::cs), x, y => if c == 'S' then (x, y) else go' cs x (y+1)

def List.uniq [BEq α] [Hashable α]: List α → List α := go {} where
  go : HashSet α → List α → List α
    | _, [] => []
    | hs, (l::ls) => if hs.contains l then go hs ls else l :: go (hs.insert l) ls

-- returns a list (n, p) where n is the step in which p was first stepped on.
def Tiles.walk (t : Tiles) (p : Pos) (steps: Nat) : (List (Nat × Pos)) × HashSet Pos := go [p] steps {} [(steps, p)] where
  go : List Pos → Nat → HashSet Pos → List (Nat × Pos) → List (Nat × Pos) × HashSet Pos
    | _, 0, vs, x => (x,vs)
    | ps, (Nat.succ n), vs, out =>
      let vs := vs.insertMany ps
      let nextPs : List Pos := ps.map (λp => p.getNeighbors t) |>.join
                               |>.filter (λp => !vs.contains p)
                               |>.uniq
      let nextOut := nextPs.map (λx => (n, x)) ++ out
      go nextPs n vs nextOut


def solve_one (s : String) :=
  let t : Tiles := s.splitOn "\n" |>.map String.toList
  let s := t.findStart
  let all := t.walk s 64
  all.1.filter (λp => p.1 % 2 == 0)
   |>.length

def main : IO Unit := do
  let f ← IO.readInputForDay 21
  println! s!"Solution One: {solve_one f}"
