import Lake
open Lake DSL

package «AdventOfCode» where
  -- add package configuration options here

lean_lib «AdventOfCode» where
  -- add library configuration options here

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
