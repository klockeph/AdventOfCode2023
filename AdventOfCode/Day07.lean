import AdventOfCode.IO
import AdventOfCode.Common
import AdventOfCode.Mergesort

def testinput := "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
".trim

def readHand (s : String) : List Char × Nat :=
  ((s.take 5).toList, (s.drop 6).toNat!)

namespace PartOne

def cards := "AKQJT98765432".toList

def isHigherCard (c1 c2 : Char) : Bool :=
  cards.indexOf! c1 < cards.indexOf! c2

def initialRank (cs : List Char) : Nat :=
  let grouped := ((cs.mergeSort isHigherCard).groupBy (. == .)).mergeSort (λl₁ l₂ => l₁.length ≥ l₂.length)
  if grouped.length == 1 then 6 else
  match (grouped.get! 0).length, (grouped.get! 1).length with
  -- | 5, 0 => 6 -- Five of a kind
  | 4, 1 => 5 -- Four of a kind
  | 3, 2 => 4 -- Full House
  | 3, 1 => 3 -- Three of a kind
  | 2, 2 => 2 -- Two pairs
  | 2, 1 => 1 -- One pair
  | 1, 1 => 0 -- High card
  | _, _ => panic! s!"Invalid hand, {String.mk cs}"

def areHigherCards : List Char → List Char → Bool
  | a::as, b::bs => if isHigherCard a b then true
                    else if isHigherCard b a then false
                    else areHigherCards as bs
  | _, _ => false
end PartOne

def addRanksToHands (hs : List (List Char × Nat)) (rankfn : (List Char → Nat)) :=
 (hs.map (λh => (h, rankfn h.1))).mergeSort (λx y => x.2 < y.2)

def sortHandsInRanks (hs : List (List Char × Nat)) (rankfn : (List Char → Nat)) (handfn : List Char → List Char → Bool) : List (List (List Char × Nat)) :=
  let hands_with_rank := addRanksToHands hs rankfn
  (hands_with_rank.groupBy (λx y => x.2 == y.2)).map (λx => (x.mergeSort (λa b => handfn b.1.1 a.1.1)).map (λ y => y.1))

def addHands (rs : List Nat) (rank : Nat) : Nat :=
  (rs.foldl (λa r => (a.1+1, a.2 + a.1 * r)) (rank, 0)).2

def addPartitionedBids (rs : List (List Nat)) (acc : Nat) (rank : Nat) : Nat :=
  match rs with
  | [] => acc
  | hs::rs => addPartitionedBids rs (acc + (addHands hs rank)) (rank + hs.length)

def solve' (l : List String) (rankfn : (List Char → Nat)) (handfn : List Char → List Char → Bool) : Nat :=
  addPartitionedBids ((sortHandsInRanks (l.map readHand) rankfn handfn).map λa => a.map λb => b.2) 0 1

def solve_one (l : List String) : Nat :=
  solve' l PartOne.initialRank PartOne.areHigherCards

-- PART TWO
-- Unfortunately it seems I have to code a lot new?

namespace PartTwo

def cards := "AKQT98765432J".toList

def isHigherCard (c1 c2 : Char) : Bool :=
  cards.indexOf! c1 < cards.indexOf! c2

def countJokers (cs : List Char) : Nat := go cs 0 where
  go : List Char → Nat → Nat
  | [], n => n
  | (c::cs), n => go cs $ n + if c == 'J' then 1 else 0

def withoutJokers : List Char → List Char
  | [] => []
  | c::cs => if c == 'J' then withoutJokers cs else c::withoutJokers cs

def initialRank (cs : List Char) : Nat :=
  let grouped := (((withoutJokers cs).mergeSort isHigherCard).groupBy (. == .)).mergeSort (λl₁ l₂ => l₁.length ≥ l₂.length)
  if grouped.length == 0 || grouped.length == 1 then 6 else
  match (grouped.get! 0).length + countJokers cs, (grouped.get! 1).length with
  -- | 5, 0 => 6 -- Five of a kind
  | 4, 1 => 5 -- Four of a kind
  | 3, 2 => 4 -- Full House
  | 3, 1 => 3 -- Three of a kind
  | 2, 2 => 2 -- Two pairs
  | 2, 1 => 1 -- One pair
  | 1, 1 => 0 -- High card
  | _, _ => panic! s!"Invalid hand, {String.mk cs}"

def areHigherCards : List Char → List Char → Bool
  | a::as, b::bs => if isHigherCard a b then true
                    else if isHigherCard b a then false
                    else areHigherCards as bs
  | _, _ => false
end PartTwo

def solve_two (l : List String) : Nat :=
  solve' l PartTwo.initialRank PartTwo.areHigherCards


def main : IO Unit := do
  let f ← String.trim <$> IO.readInputForDay 7
  let f := (f.splitOn "\n").map String.trim
  IO.println s!"Solution one: {solve_one f}"
  IO.println s!"Solution two: {solve_two f}"
