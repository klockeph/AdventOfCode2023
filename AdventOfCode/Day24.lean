import AdventOfCode.IO

structure Hail where
  x : Float
  y : Float
  z : Float
  vx : Float
  vy : Float
  vz : Float
deriving Inhabited, BEq

def parseFloatList (s : String) : List Float :=
  s.trim.splitOn "," |>.map $ Float.ofInt ∘ String.toInt! ∘ String.trim

def Hail.fromString (s : String) : Hail :=
  match s.splitOn "@" |>.map parseFloatList with
  | [[x,y,z], [vx, vy, vz]] => {x, y, z, vx, vy, vz}
  | _ => panic! "Invalid hail string."

-- Normal linear equation of the form
-- y = m*x + t
structure NormalForm2D where
  m : Float
  t : Float
deriving Inhabited, BEq

def Hail.toNormalForm2D (h : Hail) : NormalForm2D :=
  if h.vx == 0 then panic! "vx = 0" else
  let tZ := -(h.x / h.vx)
  let t := h.vy * tZ + h.y
  let m := (h.vy / h.vx)
  {t, m}

def NormalForm2D.intersects? (a b : NormalForm2D) : Option (Float × Float) :=
  if a.m == b.m ∧ b.t != a.t then none else
  let x := (b.t - a.t) / (a.m - b.m)
  let y := a.m * x + a.t
  (x,y)

def Hail.isInFuture2D (h : Hail) (p : (Float × Float)) : Bool :=
  (p.1 - h.x) * h.vx ≥ 0 ∧ (p.2 - h.y) * h.vy ≥ 0

def intersectsInFuture (a b : (Hail × NormalForm2D)) : Option (Float × Float) := do
  let p ← a.2.intersects? b.2
  if a.1.isInFuture2D p ∧ b.1.isInFuture2D p then p else none

def inBox (box : Float × Float) (p : Float × Float) : Bool :=
  p.1 ≥ box.1 ∧ p.1 ≤ box.2 ∧ p.2 ≥ box.1 ∧ p.2 ≤ box.2

def countIntersectsNf2D (box : Float × Float) (nfs : List (Hail × NormalForm2D)) (acc : Nat := 0) : Nat :=
  match nfs with
  | [] => acc
  | [_] => acc
  | (hn)::nfs =>
      let i := nfs.filterMap (intersectsInFuture hn)
      |>.filter (inBox box) -- TODO: filter inFuture!
      |>.length
    countIntersectsNf2D box nfs $ acc + i

def countIntersects2D (box : Float × Float) (hs : List Hail) : Nat :=
  countIntersectsNf2D box $ hs.map (λh => (h, h.toNormalForm2D))

def solve_one (s : String) : Nat :=
  countIntersects2D (200000000000000, 400000000000000) $
    s.trim.splitOn "\n" |>.map $ Hail.fromString ∘ String.trim

def main : IO Unit := do
  let f ← IO.readInputForDay 24
  IO.println s!"Solution One: {solve_one f}"
