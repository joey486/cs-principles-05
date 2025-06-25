import Targil5.Basic
import Targil5.AST
import Targil5.XMLParser
import Targil5.VMGenerator

open Targil5
open JackCompiler

def main (args : List String) : IO Unit := do
  match args with
  | [inputFile] => do
    let xmlContent ← IO.FS.readFile inputFile
    match parseXMLFromString xmlContent with
    | .ok xmlAst =>
      IO.println s!"Parsed XML successfully: {xmlAst}"
      IO.println "VM code generation not fully implemented in this example"
    | .error err =>
      IO.println s!"Parse error: {err}"
  | _ =>
    IO.println "Usage: jack-compiler <input.xml>"
