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

def minimumSeed (s : List Nat) (m : Nat → Nat) : Option Nat :=
  (s.map m).minimum?

def solve_one (l : List String) : Nat :=
  let s := readSeeds (l.get! 0)
  let m := readMaps (l.drop 2) id
  match minimumSeed s m with
  | Option.none => panic! "No minimum? Where are my seeds?!"
  | Option.some x => x

def seedsToIntervals : List Nat → List (Nat × Nat)
  | [] => []
  | x::y::s => (x, x + y)::seedsToIntervals s
  | _ => panic! "List length must be even"

/-
A possible idea for day two could be to coalesce the maps not into Num → Num but instead Range → Range.
In that case we could then figure out what ranges the seed ranges are mapped to and easily find the minimum.
-/

def main : IO Unit := do
  let f ← IO.readInputForDay 5
  IO.println s!"Solution One: {solve_one $ (f.splitOn "\n").map String.trim}"
  IO.println s!"Solution Two: todo"
