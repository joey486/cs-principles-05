import Lean
import Std
open System

namespace Parser

-- Define a token type
structure Token where
  tokenType : String
  tokenValue : String
deriving Repr

def parseTokenLine (line : String) : Option Token :=
  let trimmed := line.trim
  if trimmed.startsWith "<" && trimmed.endsWith ">" then
    let tagStart := trimmed.drop 1
    match tagStart.splitOn ">" with
    | tokenType :: rest =>
      let valueWithClose := String.intercalate ">" rest
      let closingTag := "</" ++ tokenType ++ ">"
      if valueWithClose.endsWith closingTag then
        let value := valueWithClose.dropRight closingTag.length |>.trim
        some ⟨tokenType, value⟩
      else none
    | _ => none
  else none

def parseInput (input : String) : List Token :=
  input.splitOn "\n" |>.filterMap parseTokenLine

abbrev ParserM := ExceptT String (StateM (List Token))

def peek : ParserM (Option Token) := do
  let s ← get
  pure s.head?

def advance : ParserM (Option Token) := do
  let s ← get
  match s with
  | [] => pure none
  | t :: ts => set ts; pure t

def expectValue (val : String) : ParserM Token := do
  let t ← advance
  match t with
  | some tk =>
    if tk.tokenValue = val then pure tk
    else throw s!"Expected token value '{val}', got '{tk.tokenValue}'"
  | none =>
      throw s!"Expected token value '{val}', but got EOF"

def expectType (ty : String) : ParserM Token := do
  let t ← advance
  match t with
  | some tk =>
    if tk.tokenType = ty then pure tk
    else throw s!"Expected type '{ty}', got '{tk.tokenType}'"
  | none => throw s!"Expected type '{ty}', but got EOF"

def indentStr (n : Nat) : String :=
  String.join (List.replicate n "  ")

def tokenToXML (t : Token) (indent : Nat) : String :=
  let i := indentStr indent
  s!"{i}<{t.tokenType}> {t.tokenValue} </{t.tokenType}>"

def isOp (s : String) : Bool :=
  s ∈ ["+", "-", "*", "/", "&", "|", "&lt;", "&gt;", "="]

def isUnaryOp (s : String) : Bool :=
  s ∈ ["-", "~"]

mutual

partial def parseExpression (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<expression>"]

  -- Parse first term
  let termLines ← parseTerm (indent + 1)
  lines := lines ++ termLines

  -- Parse optional (op term)*
  while true do
    match ← peek with
    | some t =>
      if isOp t.tokenValue then
        _ ← advance
        lines := lines ++ [tokenToXML t indent]
        let nextTermLines ← parseTerm (indent + 1)
        lines := lines ++ nextTermLines
      else break
    | none => break

  lines := lines ++ [indentStr (indent - 1) ++ "</expression>"]
  return lines

partial def parseTerm (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<term>"]

  match ← peek with
  | some t =>
    if t.tokenType ∈ ["integerConstant", "stringConstant", "keyword"] then
      _ ← advance
      lines := lines ++ [tokenToXML t indent]
    else if isUnaryOp t.tokenValue then
      -- Handle unary operators
      _ ← advance
      lines := lines ++ [tokenToXML t indent]
      let nextTerm ← parseTerm (indent + 1)
      lines := lines ++ nextTerm
    else if t.tokenValue = "(" then
      -- Handle parenthesized expressions
      let lp ← expectValue "("
      lines := lines ++ [tokenToXML lp indent]
      let expr ← parseExpression (indent + 1)
      lines := lines ++ expr
      let rp ← expectValue ")"
      lines := lines ++ [tokenToXML rp indent]
    else if t.tokenType = "identifier" then
      _ ← advance
      lines := lines ++ [tokenToXML t indent]

      -- Check for array access [expression]
      if (← peek).map (·.tokenValue) == some "[" then
        let lb ← expectValue "["
        lines := lines ++ [tokenToXML lb indent]
        let expr ← parseExpression (indent + 1)
        lines := lines ++ expr
        let rb ← expectValue "]"
        lines := lines ++ [tokenToXML rb indent]
      -- Check for subroutine call .identifier(expressionList)
      else if (← peek).map (·.tokenValue) == some "." then
        let dot ← expectValue "."
        lines := lines ++ [tokenToXML dot indent]
        let id ← expectType "identifier"
        lines := lines ++ [tokenToXML id indent]
        let lp ← expectValue "("
        lines := lines ++ [tokenToXML lp indent]
        let exprList ← parseExpressionList (indent + 1)
        lines := lines ++ exprList
        let rp ← expectValue ")"
        lines := lines ++ [tokenToXML rp indent]
      -- Check for direct subroutine call identifier(expressionList)
      else if (← peek).map (·.tokenValue) == some "(" then
        let lp ← expectValue "("
        lines := lines ++ [tokenToXML lp indent]
        let exprList ← parseExpressionList (indent + 1)
        lines := lines ++ exprList
        let rp ← expectValue ")"
        lines := lines ++ [tokenToXML rp indent]
    else
      throw s!"Unexpected token in term: {t.tokenValue}"
  | none => throw "Expected term, got EOF"

  lines := lines ++ [indentStr (indent - 1) ++ "</term>"]
  return lines

partial def parseExpressionList (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<expressionList>"]

  -- Check if empty expression list
  if (← peek).map (·.tokenValue) != some ")" then
    let expr ← parseExpression (indent + 1)
    lines := lines ++ expr

    -- Parse additional expressions separated by commas
    while (← peek).map (·.tokenValue) == some "," do
      let comma ← expectValue ","
      lines := lines ++ [tokenToXML comma indent]
      let nextExpr ← parseExpression (indent + 1)
      lines := lines ++ nextExpr

  lines := lines ++ [indentStr (indent - 1) ++ "</expressionList>"]
  return lines


partial def parseVarDec (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := []
  match ← advance with
  | some t => lines := lines ++ [tokenToXML t indent]
  | none => throw "Expected variable declaration, got EOF"
  while true do
    let next ← peek
    match next with
    | some t =>
      lines := lines ++ [tokenToXML t indent]
      _ ← advance
      if t.tokenValue = ";" then break
    | none => break
  return [indentStr (indent - 1) ++ "<varDec>"] ++ lines ++ [indentStr (indent - 1) ++ "</varDec>"]

partial def parseClassVarDec (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := []
  match ← advance with
  | some t => lines := lines ++ [tokenToXML t indent]
  | none => throw "Expected class variable declaration, got EOF"
  while true do
    let next ← peek
    match next with
    | some t =>
      lines := lines ++ [tokenToXML t indent]
      _ ← advance
      if t.tokenValue = ";" then break
    | none => break
  return [indentStr (indent - 1) ++ "<classVarDec>"] ++ lines ++ [indentStr (indent - 1) ++ "</classVarDec>"]

partial def parseParameterList (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<parameterList>"]

  -- Check if empty parameter list
  if (← peek).map (·.tokenValue) != some ")" then
    -- Parse parameters if they exist
    while (← peek).map (·.tokenValue) != some ")" do
      match ← advance with
      | some t => lines := lines ++ [tokenToXML t indent]
      | none => break

  lines := lines ++ [indentStr (indent - 1) ++ "</parameterList>"]
  return lines

partial def parseLetStatement (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<letStatement>"]

  let letKw ← expectValue "let"
  lines := lines ++ [tokenToXML letKw indent]

  let id ← expectType "identifier"
  lines := lines ++ [tokenToXML id indent]

  -- Check for array access
  if (← peek).map (·.tokenValue) == some "[" then
    let lb ← expectValue "["
    lines := lines ++ [tokenToXML lb indent]
    let expr ← parseExpression (indent + 1)
    lines := lines ++ expr
    let rb ← expectValue "]"
    lines := lines ++ [tokenToXML rb indent]

  let eq ← expectValue "="
  lines := lines ++ [tokenToXML eq indent]

  let expr ← parseExpression (indent + 1)
  lines := lines ++ expr

  let semi ← expectValue ";"
  lines := lines ++ [tokenToXML semi indent]

  lines := lines ++ [indentStr (indent - 1) ++ "</letStatement>"]
  return lines

partial def parseIfStatement (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<ifStatement>"]

  let ifKw ← expectValue "if"
  lines := lines ++ [tokenToXML ifKw indent]

  let lp ← expectValue "("
  lines := lines ++ [tokenToXML lp indent]

  let expr ← parseExpression (indent + 1)
  lines := lines ++ expr

  let rp ← expectValue ")"
  lines := lines ++ [tokenToXML rp indent]

  let lb ← expectValue "{"
  lines := lines ++ [tokenToXML lb indent]

  let stmts ← parseStatements (indent + 1)
  lines := lines ++ stmts

  let rb ← expectValue "}"
  lines := lines ++ [tokenToXML rb indent]

  -- Check for optional else clause
  if (← peek).map (·.tokenValue) == some "else" then
    let elseKw ← expectValue "else"
    lines := lines ++ [tokenToXML elseKw indent]

    let elseLb ← expectValue "{"
    lines := lines ++ [tokenToXML elseLb indent]

    let elseStmts ← parseStatements (indent + 1)
    lines := lines ++ elseStmts

    let elseRb ← expectValue "}"
    lines := lines ++ [tokenToXML elseRb indent]

  lines := lines ++ [indentStr (indent - 1) ++ "</ifStatement>"]
  return lines

partial def parseWhileStatement (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<whileStatement>"]

  let whileKw ← expectValue "while"
  lines := lines ++ [tokenToXML whileKw indent]

  let lp ← expectValue "("
  lines := lines ++ [tokenToXML lp indent]

  let expr ← parseExpression (indent + 1)
  lines := lines ++ expr

  let rp ← expectValue ")"
  lines := lines ++ [tokenToXML rp indent]

  let lb ← expectValue "{"
  lines := lines ++ [tokenToXML lb indent]

  let stmts ← parseStatements (indent + 1)
  lines := lines ++ stmts

  let rb ← expectValue "}"
  lines := lines ++ [tokenToXML rb indent]

  lines := lines ++ [indentStr (indent - 1) ++ "</whileStatement>"]
  return lines


partial def parseDoStatement (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<doStatement>"]

  let doKw ← expectValue "do"
  lines := lines ++ [tokenToXML doKw indent]

  let id ← expectType "identifier"
  lines := lines ++ [tokenToXML id indent]

  -- Check if this is a method call with dot notation or direct call
  if (← peek).map (·.tokenValue) == some "." then
    let dot ← expectValue "."
    lines := lines ++ [tokenToXML dot indent]

    let methodId ← expectType "identifier"
    lines := lines ++ [tokenToXML methodId indent]

  let lp ← expectValue "("
  lines := lines ++ [tokenToXML lp indent]

  let exprList ← parseExpressionList (indent + 1)
  lines := lines ++ exprList

  let rp ← expectValue ")"
  lines := lines ++ [tokenToXML rp indent]

  let semi ← expectValue ";"
  lines := lines ++ [tokenToXML semi indent]

  lines := lines ++ [indentStr (indent - 1) ++ "</doStatement>"]
  return lines

partial def parseReturnStatement (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<returnStatement>"]

  let retKw ← expectValue "return"
  lines := lines ++ [tokenToXML retKw indent]

  -- Check if there's an expression before the semicolon
  if (← peek).map (·.tokenValue) != some ";" then
    let expr ← parseExpression (indent + 1)
    lines := lines ++ expr

  let semi ← expectValue ";"
  lines := lines ++ [tokenToXML semi indent]

  lines := lines ++ [indentStr (indent - 1) ++ "</returnStatement>"]
  return lines



partial def parseStatements (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<statements>"]

  while true do
    match ← peek with
    | some t =>
      let val := t.tokenValue
      if val == "let" then
        let stmt ← parseLetStatement (indent + 1)
        lines := lines ++ stmt
      else if val == "if" then
        let stmt ← parseIfStatement (indent + 1)
        lines := lines ++ stmt
      else if val == "while" then
        let stmt ← parseWhileStatement (indent + 1)
        lines := lines ++ stmt
      else if val == "do" then
        let stmt ← parseDoStatement (indent + 1)
        lines := lines ++ stmt
      else if val == "return" then
        let stmt ← parseReturnStatement (indent + 1)
        lines := lines ++ stmt
      else break
    | none => break

  lines := lines ++ [indentStr (indent - 1) ++ "</statements>"]
  return lines

partial def parseSubroutineBody (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := [indentStr (indent - 1) ++ "<subroutineBody>"]

  let lb ← expectValue "{"
  lines := lines ++ [tokenToXML lb indent]

  -- Parse varDecs
  while (← peek).map (·.tokenValue) == some "var" do
    let v ← parseVarDec (indent + 1)
    lines := lines ++ v

  -- Parse statements
  let stmts ← parseStatements (indent + 1)
  lines := lines ++ stmts

  let rb ← expectValue "}"
  lines := lines ++ [tokenToXML rb indent]

  lines := lines ++ [indentStr (indent - 1) ++ "</subroutineBody>"]
  return lines
  end

partial def parseSubroutineDec (indent : Nat) : ParserM (List String) := do
  let mut lines : List String := []

  -- Parse function/method/constructor keyword
  let funcKw ← advance
  match funcKw with
  | some t => lines := lines ++ [tokenToXML t indent]
  | none => throw "Expected subroutine keyword"

  -- Parse return type
  let retType ← advance
  match retType with
  | some t => lines := lines ++ [tokenToXML t indent]
  | none => throw "Expected return type"

  -- Parse subroutine name
  let name ← expectType "identifier"
  lines := lines ++ [tokenToXML name indent]

  -- Parse opening parenthesis
  let lp ← expectValue "("
  lines := lines ++ [tokenToXML lp indent]

  -- Parse parameter list
  let params ← parseParameterList (indent + 1)
  lines := lines ++ params

  -- Parse closing parenthesis
  let rp ← expectValue ")"
  lines := lines ++ [tokenToXML rp indent]

  -- Parse subroutine body
  let body ← parseSubroutineBody (indent + 1)
  lines := lines ++ body

  return [indentStr (indent - 1) ++ "<subroutineDec>"] ++ lines ++ [indentStr (indent - 1) ++ "</subroutineDec>"]

partial def parseClass (indent : Nat) : ParserM (List String) := do
  let kw ← expectValue "class"
  let name ← expectType "identifier"
  let lb ← expectValue "{"

  let mut children : List String := [
    indentStr indent ++ "<class>",
    tokenToXML kw (indent + 1),
    tokenToXML name (indent + 1),
    tokenToXML lb (indent + 1)
  ]

  while (← peek).map (·.tokenValue) != some "}" do
    let next ← peek
    match next.map (·.tokenValue) with
    | some "static" | some "field" =>
      let dec ← parseClassVarDec (indent + 1)
      children := children ++ dec
    | some val =>
      if val ∈ ["constructor", "function", "method"] then
        let sub ← parseSubroutineDec (indent + 1)
        children := children ++ sub
      else break
    | none => break

  let rb ← expectValue "}"
  children := children ++ [tokenToXML rb (indent + 1)]
  children := children ++ [indentStr indent ++ "</class>"]
  return children

-- Parser entry point
def parser (input : String) : String :=
  let tokens := parseInput input
  match ExceptT.run (parseClass 0) |>.run tokens with
  | (Except.ok result, _) => String.intercalate "\n" result
  | (Except.error err, _) => s!"Parser error: {err}"

end Parser
