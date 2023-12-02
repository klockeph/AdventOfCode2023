
namespace IO

def toTwoDigitString (x : Nat) : String :=
  if x < 10 then s!"0{x}" else s!"{x}"

def readInputForDay (x : Nat) : IO String :=
  IO.FS.readFile s!"./AdventOfCode/data/day{toTwoDigitString x}.txt"
