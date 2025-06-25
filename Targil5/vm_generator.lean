import Std.Data.HashMap
open Std
open String

namespace VMGenerator

-- Define XMLNode for simple recursive parsing
inductive XMLNode where
  | elem : String → List XMLNode → XMLNode
  | text : String → XMLNode
  deriving Repr, Nonempty

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
  | none => default

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

-- ───  XML PARSER  ─────────────────────────────────────────────────

partial def parseTag (line : String) : Option String :=
  let trimmed := line.trim
  if trimmed.startsWith "<" && !trimmed.startsWith "</" then
    let afterLT := trimmed.drop 1
    let tagName := afterLT.takeWhile (fun c => c != '>' && c != ' ')
    if tagName.length > 0 then some tagName else none
  else none

partial def parseXMLLines (lines : List String) (indent : Nat := 0) : (XMLNode × List String) :=
  match lines with
  | [] => (XMLNode.text "", [])
  | l :: rest =>
    let spaceCount := l.length - l.trimLeft.length
    let lineIndent := spaceCount / 2

    -- If this line has less indentation than expected, we're done with this level
    if lineIndent < indent then
      (XMLNode.text "", lines)
    else
      let trimmed := l.trim
      -- If this is a closing tag, we're done
      if trimmed.startsWith "</" then
        (XMLNode.text "", lines)
      else match parseTag l with
      | some tag =>
        let closingTag := s!"</{tag}>"
        let rec parseChildren (ls : List String) (acc : List XMLNode) :=
          match ls with
          | [] => (acc.reverse, [])
          | x :: xs =>
            let xTrimmed := x.trim
            if xTrimmed == closingTag then
              (acc.reverse, xs)
            else if xTrimmed.startsWith "</" then
              -- If we hit any closing tag, stop parsing children
              (acc.reverse, ls)
            else
              let xSpaceCount := x.length - x.trimLeft.length
              let xLineIndent := xSpaceCount / 2
              -- Only parse as child if indentation is greater than current
              if xLineIndent > indent then
                let (child, rem) := parseXMLLines ls (indent + 1)
                match rem with
                | [] => (acc.reverse, [])
                | _ => parseChildren rem (child :: acc)
              else
                -- If indentation is not greater, we're done with children
                (acc.reverse, ls)
        let (children, remLines) := parseChildren rest []
        (XMLNode.elem tag children, remLines)
      | none =>
        -- Handle text content
        if trimmed.length > 0 then
          (XMLNode.text trimmed, rest)
        else
          -- Skip empty lines
          parseXMLLines rest indent

-- ─── VM WRITER ─────────────────────────────────────────────────────────

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

-- ─── HELPERS (STUBS) ───────────────────────────────────────────────────
-- Fixed helper functions to replace the stubs

def extractFunctionName (children : List XMLNode) : String :=
  let rec findFunctionName (nodes : List XMLNode) (className : String) : String :=
    match nodes with
    | [] => "Unknown.unknown"
    | (XMLNode.elem "identifier" [XMLNode.text name]) :: _ =>
      -- First identifier after "function" keyword should be the function name
      s!"{className}.{name}"
    | (XMLNode.elem _ subChildren) :: rest =>
      match findFunctionName subChildren className with
      | "Unknown.unknown" => findFunctionName rest className
      | found => found
    | _ :: rest => findFunctionName rest className

  -- First, find the class name from the parent context
  let className := "Main" -- You might want to pass this as a parameter
  findFunctionName children className

def countLocalVars (children : List XMLNode) : Nat :=
  -- Count variables in a single varDec (handles "int i, sum;" case)
  let countVarsInDec (varDecChildren : List XMLNode) : Nat :=
    let rec countIdentifiers (nodes : List XMLNode) (count : Nat) (foundType : Bool) : Nat :=
      match nodes with
      | [] => count
      | (XMLNode.elem "keyword" _) :: rest =>
        countIdentifiers rest count true -- Found type keyword like "int"
      | (XMLNode.elem "identifier" _) :: rest =>
        if foundType then
          countIdentifiers rest (count + 1) foundType
        else
          countIdentifiers rest count foundType -- This might be the type name like "Array"
      | _ :: rest => countIdentifiers rest count foundType

    countIdentifiers varDecChildren 0 false

  let rec countVarDecs (nodes : List XMLNode) : Nat :=
    match nodes with
    | [] => 0
    | (XMLNode.elem "varDec" varChildren) :: rest =>
      let varCount := countVarsInDec varChildren
      varCount + countVarDecs rest
    | (XMLNode.elem _ subChildren) :: rest =>
      countVarDecs subChildren + countVarDecs rest
    | _ :: rest => countVarDecs rest

  countVarDecs children

-- ─── CODE GENERATOR ────────────────────────────────────────────────────

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

  | XMLNode.elem "subroutineDec" children => do
    let st := SymbolTable.startSubroutine symTable
    let funcName := extractFunctionName children
    let nLocals := countLocalVars children
    function funcName nLocals
    for c in children do
      match c with
      | XMLNode.elem "statements" _ =>
        let _ ← vmGen c st
        pure ()
      | _ => pure ()
    return st

  | XMLNode.elem "letStatement" _ => do
    emit "// let statement"
    return symTable

  | XMLNode.elem "doStatement" _ => do
    emit "// do statement"
    return symTable

  | XMLNode.elem "returnStatement" _ => do
    emit "// return statement"
    ret
    return symTable

  | XMLNode.elem "ifStatement" _ => do
    emit "// if statement"
    return symTable

  | XMLNode.elem "whileStatement" _ => do
    emit "// while statement"
    return symTable

  | XMLNode.elem "statements" children => do
    let mut st := symTable
    for c in children do
      st ← vmGen c st
    return st

  | XMLNode.elem _ children => do
    let mut st := symTable
    for c in children do
      st ← vmGen c st
    return st

  | XMLNode.text _ =>
    return symTable


end VMGenerator
