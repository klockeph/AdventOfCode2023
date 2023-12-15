import AdventOfCode.IO
import AdventOfCode.Common
import Lean

def asciihash (cs : List Char) : Nat :=
  cs.foldl (λacc c => Nat.mod ((acc + c.toNat)*17) 256) 0

-- Why didn't I find |> notation earlier? :')
def solve_one (s : String) : Nat :=
  s.replace "\n" ""
  |>.splitOn ","
  |>.map (λs => asciihash s.toList)
  |>.foldl Nat.add 0

open Lean (AssocList)

-- Omg I discovered Fin! So helpful to express that all lenses should be 0 ≤ l < 9
-- 0 is unused... maybe there's a better type still?
abbrev Focal := Fin 10
abbrev Label := String
abbrev Box := AssocList Label Focal

instance : Repr Box where
  reprPrec t p := reprPrec t.toList.reverse p

def Box.insertOrUpdate (b : Box) (l : Label) (f: Focal) :=
  match b.find? l with
  | Option.none => b.insert l f
  | Option.some _ => b.replace l f

-- Fin is annoying already, coz Fin.val
-- Also how annoying is it that AssocList does not have foldr? Or length?
def Box.focalSum (b : Box) : Nat :=
  b.toList.foldr (λp acc => (acc.1 + 1, p.2.val * acc.1 + acc.2)) (1, 0)
  |>.2

-- And Fin.ofNat I guess..
def String.parseAdd (s : String) : Label × Focal :=
  let ps := s.splitOn "="
  (ps.get! 0, Fin.ofNat $ ps.get! 1 |>.toNat!)

def String.parseRemove (s : String) : Label :=
  s.dropRight 1

-- Let's try out Arrays!
abbrev Boxes := Array Box

-- More helpers, yay.
def Boxes.replaceAt! (bs : Boxes) (n : Nat) (b : Box) : Boxes :=
  bs.eraseIdx n |>.insertAt! n b

def Boxes.doAddInstr (bs : Boxes) (s : String) : Boxes :=
  let (label, focal) := s.parseAdd
  let bIdx := asciihash label.toList
  let b : Box := bs.get! bIdx
  let newB := b.insertOrUpdate label focal
  bs.replaceAt! bIdx newB

def Boxes.doRemoveInstr (bs : Boxes) (s : String) : Boxes :=
  let label := s.parseRemove
  let bIdx := asciihash label.toList
  let b : Box := bs.get! bIdx
  let newB := b.erase label
  bs.replaceAt! bIdx newB

def Boxes.doInstruction (bs : Boxes) (s : String) : Boxes :=
  if s.contains '=' then bs.doAddInstr s else
  if s.contains '-' then bs.doRemoveInstr s else
  bs

def Boxes.focalSum (bs : Boxes) : Nat :=
  bs.foldl (λacc b => (acc.1 + 1, acc.1 * b.focalSum + acc.2)) (1,0)
  |>.2

def emptyBoxes : Boxes := Array.mkArray 256 {}

def finalBoxes (s : String) : Boxes :=
  s.splitOn ","
  |>.foldl (λbs s => bs.doInstruction s) emptyBoxes

def solve_two (s : String) : Nat :=
  finalBoxes s
  |>.focalSum

def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 15
  IO.println s!"Solution one: {solve_one f}"
  IO.println s!"Solution two: {solve_two f}"
