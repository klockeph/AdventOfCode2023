import AdventOfCode.IO
import Lean.Data.HashSet
import Lean.Data.HashMap

open Lean (HashSet)
open Lean (HashMap)

abbrev Tiles := List $ List Char
abbrev Pos := Nat × Nat

def start_pos : Pos := (1, 0)
def Tiles.endPos (t : Tiles) : Pos :=
  (t.length - 2, t.length - 1)

def Tiles.at (t : Tiles) (p : Pos) : Char :=
  t.get! p.2
  |>.get! p.1

def Pos.valid (t : Tiles) (p : Pos) : Bool :=
  p.1 < t.length ∧ p.2 < t.length ∧ (t.at p ≠ '#')

def Pos.getNext (t : Tiles) (p : Pos) : List Pos :=
  (match t.at p with
    | '.' => [(p.1 + 1, p.2), (p.1 - 1, p.2), (p.1, p.2 + 1), (p.1, p.2 - 1)]
    | '>' => [(p.1 + 1, p.2)]
    | '<' => [(p.1 - 1, p.2)]
    | 'v' => [(p.1, p.2 + 1)]
    | '^' => [(p.1, p.2 - 1)]
    | _ => panic! "Invalid tile to stand on."
  ).filter λpn => pn.valid t ∧ pn ≠ p

def readInput (s : String) : Tiles :=
  s.splitOn "\n" |>.map (String.toList ∘ String.trim)

/-
At this point: Either think through and use dijkstra (??)
or just implement shitty dfs.
-/

abbrev NextFnWithLength := Pos → List (Nat × Pos)
abbrev NextFn := Pos → List Pos

partial def dfs (next_fn : NextFnWithLength) (end_pos : Pos) (vs : HashSet Pos) (p : Pos): Option Nat :=
  if p == end_pos then some 0 else
  next_fn p
  |>.filter (λa => !vs.contains a.2)
  |>.filterMap (λa => dfs next_fn end_pos (vs.insert p) a.2 |>.map ( . + a.1))
  |>.maximum?

def solve_one (s : String) : Nat :=
  let t := readInput s
  dfs (λa => (Pos.getNext t a).map (λx => (1,x))) t.endPos {} start_pos
  |>.get!

def Pos.getNextTwo (t : Tiles) (p : Pos) : List Pos :=
  [(p.1 + 1, p.2), (p.1 - 1, p.2), (p.1, p.2 + 1), (p.1, p.2 - 1)]
  |>.filter λpn => pn.valid t ∧ pn ≠ p

partial def Pos.walkUntilCrossing (next_fn : NextFn) (p : Pos) (s: Nat := 0) (lastP : Pos := (0,0)) : (Nat × Pos) :=
  match (next_fn p).filter (. ≠ lastP) with
  | [pn] => pn.walkUntilCrossing next_fn (s+1) p
  | _ => (s, p)

def Pos.walkUntilCrossings (next_fn : NextFn) (p : Pos) : List (Nat × Pos) :=
  match next_fn p with
  | [] => panic! "Started from a non-crossing?"
  | ps => ps.map (Pos.walkUntilCrossing next_fn . 1 p)
          |>.filter (λx => x.2 ≠ p)

-- represented as adjacence-lists, stored in a hashmap
abbrev Graph := HashMap Pos $ List (Nat × Pos)

partial def Tiles.parseGraph (t : Tiles) (next_fn : NextFn) (ps : List Pos := [start_pos]) (g : Graph := {}) : Graph :=
  let ps := ps.filter (!g.contains .)
  if ps.isEmpty then g else
  let nextCs := ps.map λp => (p, p.walkUntilCrossings next_fn)
  let g := nextCs.foldl (λg x => g.insert x.1 x.2) g
  let ps := (nextCs.map λx => x.2.map λy => y.2).join
  t.parseGraph next_fn ps g

def Graph.getNext (g : Graph) (p : Pos) : List (Nat × Pos) :=
  match g.find? p with
  | none => []
  | some p => p

-- Super slow...
def solve_two (s : String) : Nat :=
  let t := readInput s
  let g := t.parseGraph (Pos.getNextTwo t)
  dfs g.getNext t.endPos {} start_pos
  |>.get!

def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 23
  println! s!"Solution one: {solve_one f}"
  println! s!"Solution one: {solve_two f}"
