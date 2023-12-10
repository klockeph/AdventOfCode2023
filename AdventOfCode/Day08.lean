import AdventOfCode.IO
import Lean.Data.HashMap

open Lean (HashMap)

def readLine (s : String) : String × String × String :=
  let source := s.take 3
  let left := (s.drop 7).take 3
  let right := (s.drop 12).take 3
  (source, left, right)

def parseMap (ls : List String) : HashMap String (String × String) :=
  (ls.map readLine).foldl (λhm l => hm.insert l.1 l.2) {}

#eval (parseMap ["AAA = (AAA, BBB)"]).find? "AAA"

def parseInput (ls : List String) : (List Char) × HashMap String (String × String) :=
  let dirs := (ls.get! 0).trim.toList
  let m := parseMap $ ls.drop 2
  (dirs, m)

def step (m : HashMap String (String × String)) (loc : String) (dir : Char) : String :=
  match dir, m.find! loc with
  | 'L', (l, _) => l
  | 'R', (_, r) => r
  | _, _ => panic! "Invalid direction!"

def walk (m : HashMap String (String × String)) (dirs : List Char) (loc : String) (acc : Nat) : String × Nat :=
  match dirs with
  | [] => (loc, acc)
  | (d::ds) =>
    let newLoc := step m loc d
    if newLoc = "ZZZ" then (newLoc,acc+1)
    else walk m ds newLoc (acc+1)

partial def walkMany (m : HashMap String (String × String)) (dirs : List Char) (loc : String) (acc : Nat) : Nat :=
  let (newLoc, steps) := walk m dirs loc acc
  if newLoc = "ZZZ" then steps
  else walkMany m dirs newLoc steps

def solve_one (s: List String) : Nat :=
  let (dirs, m) := parseInput s
  walkMany m dirs "AAA" 0


def testinput := "
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
".trim

#eval solve_one $ testinput.splitOn "\n"

def main : IO Unit := do
  let f ← (String.splitOn . "\n") <$> IO.readInputForDay 8
  IO.println s!"Solution One: {solve_one f}"
