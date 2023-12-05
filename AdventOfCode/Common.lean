
def String.parseNatList (s : String) : List Nat :=
  ((s.trim.splitOn " ").map String.toNat?).filterMap id

def List.partitionWhile (l : List α) (f : α → Bool) : List α × List α :=
  let rest := l.dropWhile f
  (l.take (l.length - rest.length), rest)
