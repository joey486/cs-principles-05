/-
## Authors
Yakir Mauda - 322434549
Yossef Heifetz - 216175398
-/

import Lean
import Std
import Targil5
open System


/--
  Reads a `.xml` file, applies the translator to each line,
  and returns the list of translated ASM instructions.
--/
unsafe  def processXMLFile (filePath : FilePath) : IO (List String) := do
  let content ← IO.FS.readFile filePath
  let translated := parser content -- whole file, not split line-by-line
  return translated.splitOn "\n"
/--
  Iterates over all `.xml` files in a given directory and writes the ASM output
  to a file named after the directory itself.
--/
unsafe def writeAllFilesToVM (dirPath : FilePath) : IO Unit := do
  let dirName := dirPath.fileName.getD "output"
  let outputFile := dirPath / s!"{dirName}.vm"
  let files ← FilePath.readDir dirPath
  let vmFiles := files.filter (·.path.extension == some "xml")

  let mut allLines : List String := []

  for file in vmFiles do
    IO.println s!"Processing {file.path.fileName.getD ""}..."
    let translated ← processXMLFile file.path
    allLines := allLines ++ translated

  let finalOutput := String.intercalate "\n" (allLines.filter (· ≠ "")) ++ "\n"
  IO.FS.writeFile outputFile finalOutput
  IO.println s!"Translation complete. Output written to {outputFile}."

/--
  Entry point: asks user for directory path containing `.xml` files and begins translation.
--/
unsafe def main : IO Unit := do
  IO.print "Enter directory path: "
  let path ← (←IO.getStdin).getLine
  let cleanPath := path.dropRightWhile Char.isWhitespace
  writeAllFilesToVM cleanPath
