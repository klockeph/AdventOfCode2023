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

def step (m : HashMap String (String × String)) (loc : List String) (dir : Char) : List String :=
  loc.map λl => match dir, m.find! l with
  | 'L', (l, _) => l
  | 'R', (_, r) => r
  | _, _ => panic! "Invalid direction!"

def walk (step : List String → Char → List String) (end_con : List String → Bool) (dirs : List Char) (loc : List String) (acc : Nat) : List String × Nat :=
  match dirs with
  | [] => (loc, acc)
  | (d::ds) =>
    let newLoc := step loc d
    if end_con newLoc then (newLoc,acc+1)
    else walk step end_con ds newLoc (acc+1)

partial def walkMany (step : List String → Char → List String) (end_con : List String → Bool) (dirs : List Char) (loc : List String) (acc : Nat) : Nat :=
  let (newLoc, steps) := walk step end_con dirs loc acc
  if end_con newLoc then steps
  else walkMany step end_con dirs newLoc steps

def solve_one (s: List String) : Nat :=
  let (dirs, m) := parseInput s
  walkMany (step m) (. == ["ZZZ"]) dirs ["AAA"] 0

-- PART TWO

def solve_two (s : List String) : Nat :=
  let (dirs, m) := parseInput s
  let start_locs := m.fold (λd l _ => if l.endsWith "A" then l::d else d) []
  let end_con := λ(l : List String) => l.all (λx => x.endsWith "Z")
  walkMany (step m) end_con dirs start_locs 0

def main : IO Unit := do
  let f ← (String.splitOn . "\n") <$> IO.readInputForDay 8
  IO.println s!"Solution One: {solve_one f}"
  -- TODO: times out :(
  -- IO.println s!"Solution Two: {solve_two f}"
