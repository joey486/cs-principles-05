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

-- Debug: print XML structure (IO version)
partial def printXMLNode (node : XMLNode) (indent : String := "") : IO Unit :=
  match node with
  | XMLNode.elem tag children => do
    IO.println s!"{indent}<{tag}> ({children.length} children)"
    for child in children do
      printXMLNode child (indent ++ "  ")
    IO.println s!"{indent}</{tag}>"
  | XMLNode.text content => do
    if content.trim.length > 0 then
      IO.println s!"{indent}TEXT: '{content.trim}'"
    else
      pure ()

-- Wrapper: parse XML text to XMLNode tree
def parseXML (input : String) : VMGenerator.XMLNode :=
  let lines := input.splitOn "\n"
  let (node, _) := VMGenerator.parseXMLLines lines 0
  node

-- VM generation entry: takes XML string, parses and generates VM code lines
def generateVMFromXML (xmlText : String) : List String :=
  let rootNode := parseXML xmlText
  let initSymTable := SymbolTable.empty
  let (_, vmLines) := (VMGenerator.vmGen rootNode initSymTable).run []
  vmLines

/--
  Reads a `.xml` file, applies the parser and VM generator,
  and returns the list of VM instructions.
-/
def processXMLFile (filePath : FilePath) : IO (List String) := do
  IO.println s!"DEBUG: Reading file {filePath}"
  let content ← IO.FS.readFile filePath
  IO.println s!"DEBUG: File content length: {content.length}"
  IO.println s!"DEBUG: First 200 chars: {content.take 200}"

  -- Debug: Show parsed XML structure
  let rootNode := parseXML content
  IO.println "DEBUG: Parsed XML structure:"
  printXMLNode rootNode

  let vmCommands := generateVMFromXML content
  IO.println s!"DEBUG: Generated {vmCommands.length} VM lines:"
  for line in vmCommands do
    IO.println s!"  {line}"

  return vmCommands

/--
  Iterates over all `.xml` files in a directory, translates them,
  and writes combined VM output to a single `.vm` file named after the directory.
-/
def writeAllFilesToVM (dirPath : FilePath) : IO Unit := do
  let dirName := dirPath.fileName.getD "output"
  let outputFile := dirPath / s!"{dirName}.vm"
  IO.println s!"DEBUG: Output file will be: {outputFile}"

  let files ← FilePath.readDir dirPath
  let xmlFiles := files.filter (·.path.extension == some "xml")
  IO.println s!"DEBUG: Found {xmlFiles.size} XML files"

  let mut allLines : List String := []

  for file in xmlFiles do
    IO.println s!"Processing {file.path.fileName.getD ""}..."
    let vmLines ← processXMLFile file.path
    IO.println s!"DEBUG: Got {vmLines.length} VM lines from this file"
    allLines := allLines ++ vmLines

  IO.println s!"DEBUG: Total VM lines collected: {allLines.length}"
  let nonEmptyLines := allLines.filter (· ≠ "")
  IO.println s!"DEBUG: Non-empty VM lines: {nonEmptyLines.length}"

  let finalOutput := String.intercalate "\n" nonEmptyLines ++ "\n"
  IO.println s!"DEBUG: Final output length: {finalOutput.length}"
  IO.FS.writeFile outputFile finalOutput
  IO.println s!"Translation complete. Output written to {outputFile}."

end Main

/--
  Entry point: asks user for directory path containing `.xml` files and begins translation.
-/
def main : IO Unit := do
  IO.print "Enter directory path: "
  let path ← (← IO.getStdin).getLine
  let cleanPath := path.dropRightWhile Char.isWhitespace
  Main.writeAllFilesToVM cleanPath
