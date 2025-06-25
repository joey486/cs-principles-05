import Std.Data.HashMap
open Std
open String

namespace VMGenerator

-- Define XMLNode here (since Parser is missing)
inductive XMLNode where
  | elem : String → List XMLNode → XMLNode
  | text : String → XMLNode
  deriving Repr

-- ─── SYMBOL TABLE ─────────────────────────────────────────────────────

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
  | none   => default

abbrev Kind := String
abbrev Scope := SymbolTable

def empty : SymbolTable :=
  let countsInit := HashMap.emptyWithCapacity
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

end SymbolTable


partial def parseTag (line : String) : Option String :=
  let trimmed := line.trim
  if trimmed.startsWith "<" && !trimmed.startsWith "</" then
    let afterLT := trimmed.drop 1
    let tagName := afterLT.takeWhile (fun c => c != '>' && c != ' ')
    if tagName.length > 0 then some tagName else none
  else none

unsafe def parseXMLLines (lines : List String) (indent : Nat := 0) : (XMLNode × List String) :=
  match lines with
  | [] => (XMLNode.text "", [])
  | l :: rest =>
    let spaceCount := l.length - l.trimLeft.length
    let lineIndent := spaceCount / 2
    if lineIndent < indent then
      (XMLNode.text "", lines)
    else
      let trimmed := l.trim
      if trimmed.startsWith "</" then
        -- Closing tag, return to previous level
        (XMLNode.text "", lines)
      else match parseTag l with
      | some tag =>
        let closingTag := s!"</{tag}>"
        let rec parseChildren (ls : List String) (acc : List XMLNode) :=
          match ls with
          | [] => (acc.reverse, [])
          | x :: xs =>
            if x.trim == closingTag then
              (acc.reverse, xs)
            else
              let (child, rem) := parseXMLLines ls (indent + 1)
              parseChildren rem (child :: acc)
        let (children, remLines) := parseChildren rest []
        (XMLNode.elem tag children, remLines)
      | none =>
        -- Text node or malformed line
        (XMLNode.text trimmed, rest)



partial def vmGen (node : XMLNode) (symTable : SymbolTable) : VMWriter SymbolTable :=
  match node with
  | XMLNode.elem "class" children => do
      let mut st := symTable
      for c in children do
        match c with
        | XMLNode.elem "subroutineDec" _ =>
          st ← vmGen c st
        | _ => pure ()
      return st

  | XMLNode.elem "subroutineDec", children, symTable => do
    let st := startSubroutine symTable
    let funcName := extractFunctionName children -- You'll implement this
    let nLocals := countLocalVars children       -- You'll implement this
    function funcName nLocals
    for c in children do
      match c with
      | XMLNode.elem "statements", _ =>
          let _ ← vmGen c st
          pure ()
      | _ => pure ()
    return st

  | XMLNode.elem "letStatement", _, symTable => do
    -- Placeholder: actual implementation depends on variable kind, index, and expression
    emit "// let statement"
    return symTable

  | XMLNode.elem "doStatement", _, symTable => do
    emit "// do statement"
    return symTable

  | XMLNode.elem "returnStatement", _, symTable => do
    emit "// return statement"
    ret
    return symTable

  | XMLNode.elem "ifStatement", _, symTable => do
    emit "// if statement"
    return symTable

  | XMLNode.elem "whileStatement", _, symTable => do
    emit "// while statement"
    return symTable

  | XMLNode.elem "statements", children, symTable => do
    let mut st := symTable
    for c in children do
      st ← vmGen c st
    return st

  | XMLNode.elem _, children, symTable => do
    -- Generic fallback for other nodes
    let mut st := symTable
    for c in children do
      st ← vmGen c st
    return st

  | XMLNode.text _, symTable => return symTable

-- Helper stubs — you'll need to implement these properly
def extractFunctionName (children : List XMLNode) : String :=
  "Main.f"  -- replace with actual parsing

def countLocalVars (children : List XMLNode) : Nat :=
  0         -- count <varDec> elements inside <subroutineBody>

-- ─── VM WRITER ────────────────────────────────────────────────────────

abbrev VMWriter := StateT (List String) Id

def emit (cmd : String) : VMWriter Unit :=
  modify (· ++ [cmd])

def push (segment : String) (index : Nat) : VMWriter Unit :=
  emit s!"push {segment} {index}"

def pop (segment : String) (index : Nat) : VMWriter Unit :=
  emit s!"pop {segment} {index}"

def arithmetic (cmd : String) : VMWriter Unit :=
  emit cmd

def label (lbl : String) : VMWriter Unit :=
  emit s!"label {lbl}"

def goto (lbl : String) : VMWriter Unit :=
  emit s!"goto {lbl}"

def ifGoto (lbl : String) : VMWriter Unit :=
  emit s!"if-goto {lbl}"

def call (name : String) (nArgs : Nat) : VMWriter Unit :=
  emit s!"call {name} {nArgs}"

def function (name : String) (nLocals : Nat) : VMWriter Unit :=
  emit s!"function {name} {nLocals}"

def ret : VMWriter Unit :=
  emit "return"

end VMGenerator
