import AdventOfCode.IO
import Lean.Data.HashSet

open Lean (HashSet)

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

partial def dfs (get_next_fn : Tiles → Pos → List Pos) (t : Tiles) (vs : HashSet Pos) (p : Pos) : Option Nat :=
  if p == t.endPos then some 0 else
  get_next_fn t p
  |>.filter (!vs.contains .)
  |>.filterMap (dfs get_next_fn t $ vs.insert p)
  |>.maximum?
  |>.map (. + 1)

def solve_one (s : String) : Nat :=
  let t := readInput s
  dfs (Pos.getNext) t {} start_pos
  |>.get!

def Pos.getNextTwo (t : Tiles) (p : Pos) : List Pos :=
  [(p.1 + 1, p.2), (p.1 - 1, p.2), (p.1, p.2 + 1), (p.1, p.2 - 1)]
  |>.filter λpn => pn.valid t ∧ pn ≠ p

-- Problem: This stack-crashes when the input becomes too large.
-- Idea: Parse the map into a proper graph: Only have nodes at intersections.
def solve_two (s : String) : Nat :=
  let t := readInput s
  dfs (Pos.getNextTwo) t {} start_pos
  |>.get!

def testinput := "
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
".trim

#eval solve_one testinput
#eval solve_two testinput

def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 23
  println! s!"Solution one: {solve_one f}"
  println! s!"Solution one: <Would stack overflow>"
