import AdventOfCode.IO
import Lean.Data.HashSet

open Lean (HashSet)

inductive Dir
  | L : Dir
  | R : Dir
  | U : Dir
  | D : Dir

def connections (c : Char) : List Dir :=
  match c with
  | 'S' => [Dir.R, Dir.D] -- hardcoded; not derived from the map because I'm lazy.
  | '|' => [Dir.U, Dir.D]
  | '-' => [Dir.L, Dir.R]
  | 'L' => [Dir.U, Dir.R]
  | 'J' => [Dir.U, Dir.L]
  | '7' => [Dir.D, Dir.L]
  | 'F' => [Dir.D, Dir.R]
  | _ => panic! "Not a valid tile!"

abbrev Pos := Int × Int

def Pos.valid (pos : Pos) : Bool :=
  pos.1 ≥ 0 && pos.1 < 140 && pos.2 ≥ 0 && pos.2 < 140

def Pos.inDir (pos : Pos) (d : Dir) : Pos :=
  match d with
  | Dir.L => (pos.1 - 1, pos.2)
  | Dir.R => (pos.1 + 1, pos.2)
  | Dir.U => (pos.1, pos.2 - 1)
  | Dir.D => (pos.1, pos.2 + 1)

def Pos.neighbors (pos : Pos) (ds : List Dir) : List Pos := go ds [] where
  go : List Dir → List Pos → List Pos
  | [], ps => ps
  | d::ds, ps =>
    let newPos := pos.inDir d
    let newPs := if newPos.valid then newPos::ps else ps
    go ds newPs

-- #eval Pos.neighbors (0,1) [Dir.U, Dir.D, Dir.L, Dir.R]
-- #eval Pos.neighbors (0,139) [Dir.U, Dir.D, Dir.L, Dir.R]

def readMap (ls : List String) : (List (List Char)) :=
  ls.map String.toList

def findStart (m : List (List Char)) : Pos := go m 0 where
  go : List (List Char) → Int → Pos
    | [], _ => panic! "Start not found"
    | (l::ls), y => match go' l 0 with
      | Option.none => go ls (y+1)
      | Option.some x => (x, y)
  go' : List Char → Int → Option Int
    | [], _ => Option.none
    | (c::cs), x => if c == 'S' then x else go' cs (x+1)


def findNeighbors (m : List (List Char)) (ps : List Pos) : List Pos :=
  (ps.map (λp => p.neighbors $ connections $ (m.get! p.2.toNat).get! p.1.toNat)).join

-- returns length until cycle is closed
partial def bfs (m: List (List Char)) (v : HashSet Pos) (ps : List Pos) (acc : Nat) : (Nat × HashSet Pos) :=
  let next := (findNeighbors m ps).filter (!v.contains .)
  let visited := v.insertMany next
  if next.isEmpty then (acc, visited) else
  bfs m visited next $ acc+1

def solve_one (ls : List String) : Nat :=
  let m := readMap ls
  (bfs m {} [findStart m] 0).1


/-
TODO: This solution is invalid.
The idea was that every contained point would be surrounded by the main loop.
While that is true, it's not enough.
-/

partial def enclosedDir (m : HashSet Pos) (p : Pos) (d : Dir) : Bool :=
  if !p.valid then false else
  if m.contains p then true
  else enclosedDir m (p.inDir d) d

def enclosedDirOneStep (m : HashSet Pos) (p : Pos) (d : Dir) : Bool :=
  if m.contains p then false
  else enclosedDir m (p.inDir d) d

def enclosed (p : Pos) (m : HashSet Pos) : Bool :=
  [Dir.L, Dir.R, Dir.U, Dir.D].all (enclosedDirOneStep m p)

def countLine (circ : HashSet Pos) (l : List Char) (x : Nat) (y : Nat) : Nat :=
  match l with
  | [] => 0
  | _::cs =>
    let count := if enclosed (x, y) circ then 1 else 0
    count + countLine circ cs (x+1) y

def countEnclosed (m : HashSet Pos) (ls : List (List Char)) (y : Nat) :=
  match ls with
  | [] => 0
  | l::ls => countLine m l 0 y + countEnclosed m ls (y+1)

def solve_two (ls : List String) : Nat :=
  let m := readMap ls
  let visited := (bfs m {} [findStart m] 0).2
  countEnclosed visited m 0


def testinput := "
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
".trim.splitOn "\n"
-- for example some tiles above are surrounded by pipe in every direction but not enclosed

-- #eval solve_two testinput

-- #eval (bfs (readMap testinput) {} [(0,2)] 0).1

-- #eval findStart $ readMap testinput

def main : IO Unit := do
  let f ← IO.readInputForDay 10
  let f := f.trim.splitOn "\n"
  IO.println s!"Solution one: {solve_one f}"
