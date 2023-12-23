import Lake
open Lake DSL

package «AdventOfCode» where
  -- add package configuration options here

@[default_target]
lean_exe «adventofcode» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true

lean_exe day01 { root:= `AdventOfCode.Day01 }
lean_exe day02 { root:= `AdventOfCode.Day02 }
lean_exe day03 { root:= `AdventOfCode.Day03 }
lean_exe day04 { root:= `AdventOfCode.Day04 }
lean_exe day05 { root:= `AdventOfCode.Day05 }
lean_exe day06 { root:= `AdventOfCode.Day06 }
lean_exe day07 { root:= `AdventOfCode.Day07 }
lean_exe day08 { root:= `AdventOfCode.Day08 }
lean_exe day09 { root:= `AdventOfCode.Day09 }
lean_exe day10 { root:= `AdventOfCode.Day10 }
lean_exe day15 { root:= `AdventOfCode.Day15 }
lean_exe day16 { root:= `AdventOfCode.Day16 }
lean_exe day21 { root:= `AdventOfCode.Day21 }
lean_exe day23 { root:= `AdventOfCode.Day23 }
