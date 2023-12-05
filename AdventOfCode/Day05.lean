import AdventOfCode.IO
import AdventOfCode.Common

def readSeeds (s : String) : List Nat :=
  ((s.splitOn ":").get! 1).parseNatList

def getMapFun (dest : Nat) (source : Nat) (length : Nat) : (Nat → Option Nat) :=
  λx => if x ≥ source && x < source + length then dest + x - source else none

instance : Applicative List where
  pure := List.pure
  seq f x := List.bind f fun y => Functor.map y ( x ())

def mapOrId (m : List (Nat → Option Nat)) (x : Nat) : Nat :=
  match (m <*> [x]).findSome? id with
  | Option.none => x
  | Option.some y => y

def readMap (l : List String) (m : List (Nat → Option Nat)) : (Nat → Nat) :=
  match l with
  | [] => mapOrId m
  | l::ls =>
    let nats := l.parseNatList
    readMap ls $ (getMapFun (nats.get! 0) (nats.get! 1) (nats.get! 2))::m

-- #eval readMap ["50 98 2", "52 50 48"] [] $ 57

def readMapWithHeader (l : List String) : (Nat → Nat) × List String :=
  let (m, rest) := (l.drop 1).partitionWhile (λl => l.length > 0 && l.front.isDigit)
  (readMap m [], rest.drop 1)

-- TODO: show termination by showing that `readMapWithHeader` at least drops 1.
partial def readMaps : (List String) → (Nat → Nat) → (Nat → Nat)
  | [], f => f
  | l, f =>
    let (f', l') := readMapWithHeader l
    readMaps l' $ f' ∘ f

-- #eval (readMaps ["light:", "50 98 2", "", "field:", "0 15 37"] id) 51

def solve_one (l : List String) : Nat :=
  let s := readSeeds (l.get! 0)
  let m := readMaps (l.drop 2) id
  match (s.map m).minimum? with
  | Option.none => panic! "No minimum? Where are my seeds?!"
  | Option.some x => x

def main : IO Unit := do
  let f ← IO.readInputForDay 5
  IO.println s!"Solution One: {solve_one $ (f.splitOn "\n").map String.trim}"
