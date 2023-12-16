import AdventOfCode.IO
import Lean.Data.HashSet

open Lean (HashSet)

def testinput := "
.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....
".trim

def readInput (s : String) : List (List Char) :=
  s.splitOn "\n"
  |>.map (String.trim . |>.toList)

-- (x, y)
abbrev Pos := Int × Int

inductive Dir
  | Right : Dir
  | Left : Dir
  | Up : Dir
  | Down : Dir
deriving BEq, Hashable, Repr

-- d: Direction in which the incoming laser beam entered the tile.
def nextPosDir : Char → Dir → List Dir
  | '.', d => [d]
  | '|', Dir.Up => [Dir.Up]
  | '|', Dir.Down => [Dir.Down]
  | '|', _ => [Dir.Down, Dir.Up]
  | '-', Dir.Left => [Dir.Left]
  | '-', Dir.Right => [Dir.Right]
  | '-', _ => [Dir.Right, Dir.Left]
  | '/', Dir.Left => [Dir.Down]
  | '/', Dir.Right => [Dir.Up]
  | '/', Dir.Down => [Dir.Left]
  | '/', Dir.Up => [Dir.Right]
  | '\\', Dir.Left => [Dir.Up]
  | '\\', Dir.Right => [Dir.Down]
  | '\\', Dir.Down => [Dir.Right]
  | '\\', Dir.Up => [Dir.Left]
  | t, _ => panic! s!"invalid tile: {t}"

def List.getTile (t : List (List Char)) (p : Pos) : Char :=
  t.get! p.2.toNat
  |>.get! p.1.toNat

def Pos.step (p : Pos) : Dir → Pos
  | Dir.Right => (p.1 + 1, p.2)
  | Dir.Left => (p.1 - 1, p.2)
  | Dir.Up => (p.1, p.2 - 1)
  | Dir.Down => (p.1, p.2 + 1)

def Pos.onGrid (p : Pos) (t : List (List Char)) : Bool :=
  p.2 < t.length ∧ p.2 ≥ 0 ∧ p.1 < (t.get! 0).length ∧ p.1 ≥ 0

def expandFrontier (t : List (List Char)) (p : Pos) (d : Dir) : List (Pos × Dir) :=
  nextPosDir (t.getTile p) d
  |>.map (λd => (p.step d, d))
  |>.filter (λp => p.1.onGrid t)

structure BfsState where
  visited : HashSet (Pos × Dir)
  frontier : List (Pos × Dir)

def startPos : (Pos × Dir) := ((0,0), Dir.Right)

def startState : BfsState := {visited := ({} : HashSet (Pos × Dir)).insert startPos, frontier := [startPos]}

partial def bfs (t : List (List Char)) (s : BfsState) : HashSet (Pos × Dir) :=
  if s.frontier.isEmpty then s.visited else
  let nextTiles := s.frontier.map (λf => expandFrontier t f.1 f.2)
    |>.join
    |>.filter (λt => ! s.visited.contains t)
  bfs t {visited := s.visited.insertMany nextTiles, frontier := nextTiles}

def solve_one (s : String) :=
  let t := readInput s
  let posdirs := (bfs t startState)
  ({} : HashSet Pos).insertMany (posdirs.toList.map Prod.fst)
  |>.size

def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 16
  println! s!"Solution One: {solve_one f}"
