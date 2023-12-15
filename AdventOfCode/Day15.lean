import AdventOfCode.IO

def asciihash (cs : List Char) : Nat := go cs 0 where
  go : List Char → Nat → Nat
  | [], x => x
  | (c::cs), x => go cs (Nat.mod ((x + c.toNat)*17) 256)

def solve_one (s : String) : Nat :=
  (((s.replace "\n" "").splitOn ",").map (λs => asciihash s.toList)).foldl Nat.add 0

def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 15
  IO.println s!"Solution one: {solve_one f}"
