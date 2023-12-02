import AdventOfCode.IO

def lines (s : String) : List String :=
  s.splitOn "\n"

structure Balls where
  red: Nat := 0
  green: Nat := 0
  blue: Nat := 0
deriving Repr, Inhabited

instance : Max Balls where
  max b1 b2 := {
    red := max b1.red b2.red,
    green := max b1.green b2.green,
    blue := max b1.blue b2.blue
  }

instance : Add Balls where
  add b1 b2 := {
    red := b1.red + b2.red,
    green := b1.green + b2.green,
    blue := b1.blue + b2.blue
  }

-- `instance : HasSubset Balls` ... doesn't work :(

def Balls.isSubsetOf (b1 : Balls) (b2 : Balls) : Bool :=
  b1.red ≤ b2.red && b1.green ≤ b2.green && b1.blue ≤ b2.blue

def Balls.singleFromString! (s : String) : Balls :=
  match (s.dropWhile (λa => a == ' ')).splitOn " " with
  | c::["red"] => {red := c.toNat!}
  | c::["blue"] => {blue := c.toNat!}
  | c::["green"] => {green := c.toNat!}
  | _ => panic s!"{s} invalid"

-- #eval Balls.singleFromString! "  2 red"

-- why does `foldl (+) {}` not work?`
def Balls.fromString! (s : String) : Balls :=
  ((s.splitOn ",").map Balls.singleFromString!).foldl (λa b => a+b) {}

-- #eval Balls.fromString! "2 red, 2 blue, 5 green"

structure Game where
  id : Nat
  draws: List Balls
deriving Repr, Inhabited

def Game.fromString! (s : String) : Game :=
  match s.splitOn ":" with
  | game::[balls] => match game.splitOn " " with
    | "Game"::[num] => { id := num.toNat!, draws := (balls.splitOn ";").map Balls.fromString!}
    | _ => panic! s!"{game} invalid game"
  | _ => panic s!"{s} invalid line (multiple colons?)"

def Game.isValid (g: Game)  (max_balls : Balls) : Bool :=
  g.draws.all (λ b => b.isSubsetOf max_balls)

def balls_one : Balls := {red := 12, green := 13, blue:= 14}

def solve_one (gs : List Game) : Nat :=
  let valid_games := (gs.filter (λg => g.isValid balls_one))
  (valid_games.map (λg => g.id)).foldl Nat.add 0

def Balls.power (b : Balls) :=
  b.red * b.green * b.blue

def solve_two (gs : List Game) : Nat :=
  let max_balls := gs.map (λg => g.draws.foldl max {})
  (max_balls.map Balls.power).foldl Nat.add 0

def main : (IO Unit) := do
  let input ← IO.readInputForDay 2
  let games := (lines input).map Game.fromString!
  IO.println $ solve_one games
  IO.println $ solve_two games
