import Std.Data.HashMap
import Targil5.xml_node
import Targil5.vm_writer
open Std
open VMGenerator.XMLNode

namespace VMGenerator

structure SymbolInfo where
  typ : String
  kind : String -- "static", "field", "arg", "var"
  index : Nat
  deriving Repr

structure SymbolTable where
  classScope : HashMap String SymbolInfo
  subroutineScope : HashMap String SymbolInfo
  counts : HashMap String Nat
  deriving Repr

namespace SymbolTable

def HashMap.findD {α β} [BEq α] [Hashable α] (m : HashMap α β) (key : α) (default : β) : β :=
  match m.get? key with
  | some v => v
  | none => default

abbrev Kind := String
abbrev Scope := SymbolTable

def empty : SymbolTable :=
  let countsInit :=
  (HashMap.emptyWithCapacity 4)
    |>.insert "static" 0
    |>.insert "field" 0
    |>.insert "arg" 0
    |>.insert "var" 0
  { classScope := HashMap.emptyWithCapacity,
    subroutineScope := HashMap.emptyWithCapacity,
    counts := countsInit }

def startSubroutine (st : SymbolTable) : SymbolTable :=
  let countsReset := st.counts.insert "arg" 0 |>.insert "var" 0
  { st with
    subroutineScope := HashMap.emptyWithCapacity,
    counts := countsReset }

def define (st : SymbolTable) (name typ kind : String) : SymbolTable :=
  let idx := HashMap.findD st.counts kind 0
  let info := SymbolInfo.mk typ kind idx
  let updatedCounts := st.counts.insert kind (idx + 1)
  match kind with
  | "static" | "field" =>
    { st with
      classScope := st.classScope.insert name info,
      counts := updatedCounts }
  | "arg" | "var" =>
    { st with
      subroutineScope := st.subroutineScope.insert name info,
      counts := updatedCounts }
  | _ => st

def varCount (st : SymbolTable) (kind : String) : Nat :=
  HashMap.findD st.counts kind 0

def kindOf? (st : SymbolTable) (name : String) : Option String :=
  match st.subroutineScope.get? name with
  | some info => some info.kind
  | none => st.classScope.get? name |>.map (·.kind)

def typeOf? (st : SymbolTable) (name : String) : Option String :=
  match st.subroutineScope.get? name with
  | some info => some info.typ
  | none => st.classScope.get? name |>.map (·.typ)

def indexOf? (st : SymbolTable) (name : String) : Option Nat :=
  match st.subroutineScope.get? name with
  | some info => some info.index
  | none => st.classScope.get? name |>.map (·.index)

-- Helper function to collect variable declarations from subroutine body
partial def collectVarDecs (node : XMLNode) : Nat :=
  match node with
  | XMLNode.elem "subroutineBody" children =>
    children.foldl (fun acc child =>
      match child with
      | XMLNode.elem "varDec" varChildren =>
        -- Count how many variables are declared in this varDec
        let varNames := varChildren.filter (fun c =>
          match c with
          | XMLNode.elem "identifier" _ => true
          | _ => false)
        acc + varNames.length
      | _ => acc) 0
  | _ => 0

-- Forward declarations for the compile functions
partial def compileLetStatementWithTable (_ : XMLNode) (_ : SymbolTable) : VMWriter Unit := do
  emit "// let statement compilation"

partial def compileDoStatementWithTable (_ : XMLNode) (_ : SymbolTable) : VMWriter Unit := do
  emit "// do statement compilation"

partial def compileReturnStatementWithTable (_ : XMLNode) (_ : SymbolTable) : VMWriter Unit := do
  emit "// return statement compilation"

partial def compileWhileStatementWithTable (_ : XMLNode) (_ : SymbolTable) : VMWriter Unit := do
  emit "// while statement compilation"

partial def compileIfStatementWithTable (_ : XMLNode) (_ : SymbolTable) : VMWriter Unit := do
  emit "// if statement compilation"

partial def compileStatementsWithTable (node : XMLNode) (st : SymbolTable) : VMWriter Unit := do
  let statementsNode :=
    match node with
    | XMLNode.elem "subroutineBody" children =>
      children.find? (fun c =>
        match c with
        | XMLNode.elem "statements" _ => true
        | _ => false) |>.getD (XMLNode.elem "statements" [])
    | _ => XMLNode.elem "statements" []

  match statementsNode with
  | XMLNode.elem "statements" children =>
    for stmt in children do
      match stmt with
      | XMLNode.elem "letStatement" _ => compileLetStatementWithTable stmt st
      | XMLNode.elem "doStatement" _ => compileDoStatementWithTable stmt st
      | XMLNode.elem "returnStatement" _ => compileReturnStatementWithTable stmt st
      | XMLNode.elem "whileStatement" _ => compileWhileStatementWithTable stmt st
      | XMLNode.elem "ifStatement" _ => compileIfStatementWithTable stmt st
      | _ => emit "// Unsupported statement"
  | _ => pure ()

partial def compileSubroutine (className : String) (node : XMLNode) : VMWriter Unit := do
  let (_, subName, varCount) :=
    match node with
    | XMLNode.elem "subroutineDec" (XMLNode.elem "keyword" [XMLNode.text kind] :: _ :: XMLNode.elem "identifier" [XMLNode.text name] :: _ :: body :: _) =>
      let vars := collectVarDecs body
      (kind, name, vars)
    | _ => ("function", "unknown", 0)

  -- Create symbol table for this subroutine, filling argument and var symbols
  let st := SymbolTable.empty -- You'd want to build it properly here

  emit s!"function {className}.{subName} {varCount}"

  match node with
  | XMLNode.elem "subroutineDec" children =>
    match children with
    | _ :: _ :: _ :: _ :: body :: _ => compileStatementsWithTable body st
    | _ => pure ()
  | _ => pure ()

end SymbolTable
end VMGenerator
