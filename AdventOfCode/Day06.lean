import AdventOfCode.IO
import AdventOfCode.Common

def length (t : Nat) (b : Nat) : Nat :=
  b * (t - b)

-- brute force but uhhh whatevs
def variance (t : Nat) (d : Nat) : Nat :=
  (((List.range t).map (length t)).filter (. > d)).length

def List.parse_one (s : List String) : List (Nat × Nat) :=
  if s.length ≠ 2 then panic! s!"Can't parse {s}" else
  let time := (((s.get! 0).splitOn ":").get! 1).parseNatList
  let dist := (((s.get! 1).splitOn ":").get! 1).parseNatList
  List.zip time dist

def solve_one (s : List String) : Nat :=
  (s.parse_one.map (uncurry variance)).foldl Nat.mul 1

-- (b * (t-b)) > d ==> -b^2 + t*b - d > 0
-- So find the roots and take their distance.
def variance_smarter (t : Float) (d : Float) : Nat :=
  let r1 := -(1 : Float)/(2 : Float) * (t + Float.sqrt (t^2 - (4 : Float) * d))
  let r2 := -(1 : Float)/(2 : Float) * (t - Float.sqrt (t^2 - (4 : Float) * d))
  let (r1, r2) := if r1 ≤ r2 then (r1, r2) else (r2, r1)
  (r2.ceil - r1.floor).toUInt64.toNat - 1

def solve_two (s : List String) : Nat :=
  let (t, d) := (s.map (λs => s.replace " " "")).parse_one.get! 0
  variance_smarter t.toFloat d.toFloat

def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 6
  let f := f.splitOn "\n"
  IO.println s!"Solution one: {solve_one f}"
  IO.println s!"Solution two: {solve_two f}"
