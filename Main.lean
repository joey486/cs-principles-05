/-
## Authors
Yakir Mauda - 322434549
Yossef Heifetz - 216175398
-/

import Lean
import Std
import Targil5

namespace Main

open System
open Parser
open VMGenerator


-- Stub for VM generation function: replace with your actual VM generator logic
-- Wrapper: parse XML text to XMLNode tree
unsafe def parseXML (input : String) : VMGenerator.XMLNode :=
  let lines := input.splitOn "\n"
  let (node, _) := VMGenerator.parseXMLLines lines 0
  node
-- VM generation entry: takes XML string, parses and generates VM code lines
unsafe def generateVMfromXML (xmlText : String) : List String :=
  let rootNode := parseXML xmlText
  let initSymTable := SymbolTable.empty
  let (finalSymTable, vmLines) := (VMGenerator.vmGen rootNode initSymTable).run []
  vmLines

/--
  Reads a `.xml` file, applies the parser and VM generator,
  and returns the list of VM instructions.
-/
unsafe def processXMLFile (filePath : FilePath) : IO (List String) := do
  let content ← IO.FS.readFile filePath
  let parsedXML := parser content
  let vmCommands := generateVMfromXML parsedXML
  return vmCommands

/--
  Iterates over all `.xml` files in a directory, translates them,
  and writes combined VM output to a single `.vm` file named after the directory.
-/
unsafe def writeAllFilesToVM (dirPath : FilePath) : IO Unit := do
  let dirName := dirPath.fileName.getD "output"
  let outputFile := dirPath / s!"{dirName}.vm"
  let files ← FilePath.readDir dirPath
  let xmlFiles := files.filter (·.path.extension == some "xml")

  let mut allLines : List String := []

  for file in xmlFiles do
    IO.println s!"Processing {file.path.fileName.getD ""}..."
    let vmLines ← processXMLFile file.path
    allLines := allLines ++ vmLines

  let finalOutput := String.intercalate "\n" (allLines.filter (· ≠ "")) ++ "\n"
  IO.FS.writeFile outputFile finalOutput
  IO.println s!"Translation complete. Output written to {outputFile}."

/--
  Entry point: asks user for directory path containing `.xml` files and begins translation.
-/
unsafe def main : IO Unit := do
  IO.print "Enter directory path: "
  let path ← (← IO.getStdin).getLine
  let cleanPath := path.dropRightWhile Char.isWhitespace
  writeAllFilesToVM cleanPath
