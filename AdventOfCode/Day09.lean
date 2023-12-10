import AdventOfCode.IO
import AdventOfCode.Common

partial def convergence (l : List Int) : Int :=
  if l.all (. == 0) then 0 else
  let new_nums := (l.foldl (λacc l => (l, (l - acc.1)::acc.2)) (0, [])).2.reverse.drop 1
  (l.reverse.get! 0) + (convergence new_nums)

def solve_one (ls : List String) : Int :=
  (ls.map (λl => convergence (l.parseIntList))).foldl Int.add 0

def solve_two (ls : List String) : Int :=
  (ls.map (λl => convergence (l.parseIntList).reverse)).foldl Int.add 0

def main : IO Unit := do
  let f ← (λl => (l.trim.splitOn "\n").map String.trim) <$> IO.readInputForDay 9
  IO.println s!"Solution One: {solve_one f}"
  IO.println s!"Solution Two: {solve_two f}"
