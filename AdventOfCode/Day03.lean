import AdventOfCode.IO
import Lean.Data.HashMap
import Lean.Data.HashSet

def lines (s : String ) : List String :=
  s.splitOn "\n"

def Char.isSymbol (c : Char) : Bool :=
  !c.isDigit && c != '.'

def String.partition (s : String) (n : Nat) : String × String :=
  (s.take n, s.drop n)

def String.partitionUntil (s : String) (f : Char → Bool) : String × String :=
  let rest := s.dropWhile (λx => !f x)
  (s.take (s.length - rest.length), rest)

def String.partitionWhile (s : String) (f : Char → Bool) : String × String :=
  let rest := s.dropWhile f
  (s.take (s.length - rest.length), rest)

def String.last! (s : String) : Char :=
  (String.mk s.toList.reverse).get! 0

structure ThreeLines where
  prev : String
  curr : String
  next : String
deriving Repr, Inhabited

def ThreeLines.isEmpty (t : ThreeLines) : Bool :=
  t.curr.isEmpty

def ThreeLines.advanceToDigit (t : ThreeLines) (b : Bool) : ThreeLines × Bool :=
  if t.isEmpty then (t, b) else
  if (t.curr.get! 0).isDigit then (t, b) else
  let (currDropped, curr) := t.curr.partitionUntil Char.isDigit
  let (prevDropped, prev) := t.prev.partition currDropped.length
  let (nextDropped, next) := t.next.partition currDropped.length
  let hadSymbolLast := prevDropped.last!.isSymbol
                           || currDropped.last!.isSymbol
                           || nextDropped.last!.isSymbol
  ({prev, curr, next}, hadSymbolLast)

-- TODO: proofs that after calling any of those methods, ThreeLines will have the same length
-- As long as they had the same length before.

def ThreeLines.readNumber (t : ThreeLines) : ThreeLines × Nat × Bool × Bool :=
  if t.curr.isEmpty then panic! "readNumber on empty String!" else
  if !(t.curr.get! 0).isDigit then panic! "readNumber on String not starting with Num!" else
  let (currNumber, curr) := t.curr.partitionWhile Char.isDigit
  let (prevDuring, prev) := t.prev.partition currNumber.length
  let (nextDuring, next) := t.next.partition currNumber.length
  let hadSymbolDuring := prevDuring.toList.any Char.isSymbol
                      || nextDuring.toList.any Char.isSymbol
  let hadSymbolLast := prevDuring.last!.isSymbol || nextDuring.last!.isSymbol
  let num := currNumber.toNat!
  ({prev, curr, next}, num, hadSymbolDuring, hadSymbolLast)

def ThreeLines.isNextSymbol (t : ThreeLines) : Bool :=
  if t.curr.isEmpty then false else
     (t.curr.get! 0).isSymbol
  || (t.prev.get! 0).isSymbol
  || (t.next.get! 0).isSymbol

partial def ThreeLines.addNumbers (t : ThreeLines) (hadSymbolBefore : Bool) : Nat :=
  if t.isEmpty then 0 else
  let (t, hadSymbolBefore) := t.advanceToDigit hadSymbolBefore
  if t.isEmpty then 0 else
  let (t, num, hadSymbolDuring, hadSymbolLast) := t.readNumber
  let hadSymbolAfter := t.isNextSymbol
  let num_to_add := if hadSymbolBefore || hadSymbolDuring || hadSymbolAfter then num else 0
  num_to_add + t.addNumbers hadSymbolLast

-- #eval ({prev := ".....", curr := "..296", next := "5...."} : ThreeLines).addNumbers false
-- #eval ({prev := "..296", curr := "5....", next := "....."} : ThreeLines).addNumbers false

def dotString (n : Nat) : String :=
  String.mk $ List.replicate n '.'

def makeThreeLines (lines : List String) : Option ThreeLines :=
  match lines with
  | prev::curr::next::_ => Option.some {prev, curr, next}
  | prev::curr::_ => Option.some {prev, curr, next := dotString curr.length}
  | _ => Option.none

def solve_one_aux (lines : List String) :=
  match lines with
  | [] => 0
  | (_::ls) => match makeThreeLines lines with
      | Option.none => 0
      | Option.some t => t.addNumbers false + solve_one_aux ls

def solve_one : List String → Nat
  | [] => 0
  | ls@(curr::_) => solve_one_aux $ (dotString curr.length)::ls


-- ok fuck this let's use coordinates and kinda bruteforce...

open Lean (HashMap)
open Lean (HashSet)

def Coordinates := HashMap (Int × Int) Nat

def Coordinates.addCoords (c : Coordinates) (num : Nat) (y : Nat) (xStart : Nat) (toAdd : Nat) : Coordinates :=
  match toAdd with
  | 0 => c
  | Nat.succ toAdd =>
  Coordinates.addCoords (c.insert (xStart, y) num) num y (xStart+1) toAdd

partial def numberCoordinatesInLine (l : String) (y : Nat) (x : Nat) (c : Coordinates) : Coordinates :=
  if l.isEmpty then c else
  let (prev, l) := l.partitionUntil Char.isDigit
  if l.isEmpty then c else
  let x := x + prev.length
  let (numS, after) := l.partitionWhile Char.isDigit
  let num := numS.toNat!
  numberCoordinatesInLine after y (x + numS.length) (c.addCoords num y x numS.length)
-- termination_by numberCoordinatesInLine l _ _ _ => l.length

def numberCoordinates (ls : List String) (y : Nat) (c : Coordinates) : Coordinates :=
  match ls with
  | [] => c
  | (l::ls) => numberCoordinates ls (y+1) (numberCoordinatesInLine l y 0 c)

#eval (numberCoordinates ["..1.23.", "..12345."] 1 Lean.mkHashMap).toList

def asteriskCoordinatesInLine (l : List Char) (y : Nat) (x : Nat) (c : HashSet (Int × Int)) : HashSet (Int × Int) :=
  match l with
  | [] => c
  | chr::cs =>
    let newCoords := if chr == '*' then c.insert (x,y) else c
    asteriskCoordinatesInLine cs y (x+1) newCoords

def asteriskCoordinates (ls : List String) (y : Nat) (c : HashSet (Int × Int)) : HashSet (Int × Int) :=
  match ls with
  | [] => c
  | (l::ls) => asteriskCoordinates ls (y+1) (asteriskCoordinatesInLine l.toList y 0 c)

#eval (asteriskCoordinates ["..1*23.", "..12345."] 1 Lean.mkHashSet).toList

def adjacentCoords (as : (Int × Int)) : List (Int × Int) :=
  [(as.1 - 1, as.2 - 1),
   (as.1, as.2 - 1),
   (as.1 - 1, as.2),
   (as.1 + 1, as.2),
   (as.1 + 1, as.2 + 1),
   (as.1, as.2 + 1),
   (as.1 - 1, as.2 + 1),
   (as.1 + 1, as.2 - 1)]

def List.reduceOption (a : List (Option α)) : List α := a.filterMap id

def adjacentNumbers (as : (Int × Int)) (cs : Coordinates) : HashSet Nat :=
  let nums := ((adjacentCoords as).map λc => cs.find? c).reduceOption
  HashSet.empty.insertMany nums

def gearNumber (nums : HashSet Nat) : Nat :=
  if nums.size ≠ 2 then 0
  else nums.fold (λa b => a*b) 1

def gearNumbers (as : List (Int × Int)) (cs : Coordinates) (g : Nat) : Nat :=
  match as with
  | [] => g
  | a::as =>
    let num := gearNumber $ adjacentNumbers a cs
    gearNumbers as cs (g+num)

def solve_two (ls : List String) : Nat :=
  let numCoords := numberCoordinates ls 0 Lean.mkHashMap
  let asCoords := asteriskCoordinates ls 0 Lean.mkHashSet
  gearNumbers asCoords.toList numCoords 0

def main : (IO Unit) := do
  let f ← IO.readInputForDay 3
  let f := (lines f).map String.trim
  IO.println s!"Solution One: {solve_one f}"
  IO.println s!"Solution Two: {solve_two f}"
