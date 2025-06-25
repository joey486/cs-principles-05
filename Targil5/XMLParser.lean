import Targil5.Basic
import Targil5.AST

namespace Targil5

-- Simple XML node representation
inductive XMLNode where
  | element (tag : String) (content : List XMLNode)
  | text (content : String)
deriving Repr

-- Add ToString instance for XMLNode
instance : ToString XMLNode where
  toString node :=
    let rec toStringAux : XMLNode → String
      | .element tag content => s!"<{tag}>{(content.map toStringAux).foldl (· ++ ·) ""}</{tag}>"
      | .text content => content
    toStringAux node

-- Basic string parsing utilities (since we can't use Parsec)
def String.takeWhile (s : String) (p : Char → Bool) : String :=
  let chars := s.toList.takeWhile p
  String.mk chars

def String.dropWhile (s : String) (p : Char → Bool) : String :=
  let chars := s.toList.dropWhile p
  String.mk chars

def String.isWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- Helper function to find substring position
def String.findSubstring (s : String) (sub : String) : Option String.Pos :=
  let sChars := s.toList
  let subChars := sub.toList
  let rec findAt (pos : Nat) (remaining : List Char) : Option String.Pos :=
    match remaining with
    | [] => none
    | _ :: rest =>
      if remaining.take subChars.length == subChars then
        some ⟨pos⟩
      else
        findAt (pos + 1) rest
  findAt 0 sChars

-- Simple XML parsing functions using basic string operations
def parseXMLFromString (input : String) : Except String XMLNode :=
  parseElement input.trim

where
  parseElement (s : String) : Except String XMLNode := do
    if s.isEmpty then
      .error "Empty input"
    else if s.front != '<' then
      .ok (.text s.trim)
    else
      let tagEndPos := s.toList.findIdx (· == '>')
      if tagEndPos >= s.length then
        .error "Unclosed tag"
      else
        let tagContent := s.extract ⟨1⟩ ⟨tagEndPos⟩
        let tag := tagContent.trim
        let remaining := s.extract ⟨tagEndPos + 1⟩ s.endPos

        -- Simple parsing - would need more sophisticated logic for real XML
        if tag.startsWith "/" then
          .error "Unexpected closing tag"
        else
          let closeTag := s!"</{tag}>"
          let closePos := String.findSubstring remaining closeTag
          match closePos with
          | none => .ok (.element tag [.text remaining.trim])
          | some cpos =>
            let content := remaining.extract ⟨0⟩ cpos
            .ok (.element tag [.text content.trim])

-- Parse specific Jack constructs from XML
def parseJackType (xml : XMLNode) : Except String JackType :=
  match xml with
  | .element "keyword" [.text "int"] => .ok JackType.int
  | .element "keyword" [.text "char"] => .ok JackType.char
  | .element "keyword" [.text "boolean"] => .ok JackType.boolean
  | .element "keyword" [.text "void"] => .ok JackType.void
  | .element "identifier" [.text name] => .ok (JackType.className name)
  | _ => .error s!"Invalid type: {xml}"

def parseExpression (xml : XMLNode) : Except String Expression :=
  match xml with
  | .element "expression" [term] => parseTermExpression term
  | .element "expression" content => parseBinaryExpression content
  | _ => .error s!"Invalid expression: {xml}"

where
  parseTermExpression (xml : XMLNode) : Except String Expression :=
    match xml with
    | .element "term" [.element "integerConstant" [.text value]] =>
      match value.toInt? with
      | some i => .ok (Expression.intConstant i)
      | none => .error s!"Invalid integer: {value}"
    | .element "term" [.element "stringConstant" [.text value]] =>
      .ok (Expression.stringConstant value)
    | .element "term" [.element "keyword" [.text kw]] =>
      .ok (Expression.keywordConstant kw)
    | .element "term" [.element "identifier" [.text name]] =>
      .ok (Expression.varName name)
    | .element "term" [.element "identifier" [.text name],
                       .element "symbol" [.text "["],
                       index,
                       .element "symbol" [.text "]"]] => do
      let indexExpr ← parseExpression index
      .ok (Expression.arrayAccess name indexExpr)
    | .element "term" [.element "symbol" [.text op], expr] => do
      let expression ← parseTermExpression expr
      .ok (Expression.unaryOp op expression)
    | _ => .error s!"Invalid term: {xml}"

  parseBinaryExpression (content : List XMLNode) : Except String Expression :=
    match content with
    | [left, .element "symbol" [.text op], right] => do
      let leftExpr ← parseTermExpression left
      let rightExpr ← parseTermExpression right
      .ok (Expression.binaryOp leftExpr op rightExpr)
    | _ => .error s!"Invalid binary expression: {toString content}"

def parseStatement (xml : XMLNode) : Except String Statement :=
  match xml with
  | .element "letStatement" content => parseLetStatement content
  | .element "ifStatement" content => parseIfStatement content
  | .element "whileStatement" content => parseWhileStatement content
  | .element "doStatement" content => parseDoStatement content
  | .element "returnStatement" content => parseReturnStatement content
  | _ => .error s!"Invalid statement: {xml}"

where
  parseLetStatement (content : List XMLNode) : Except String Statement :=
    match content with
    | [.element "keyword" [.text "let"],
       .element "identifier" [.text varName],
       .element "symbol" [.text "="],
       expr,
       .element "symbol" [.text ";"]] => do
      let expression ← parseExpression expr
      .ok (Statement.letStatement varName none expression)
    | [.element "keyword" [.text "let"],
       .element "identifier" [.text varName],
       .element "symbol" [.text "["],
       indexExpr,
       .element "symbol" [.text "]"],
       .element "symbol" [.text "="],
       valueExpr,
       .element "symbol" [.text ";"]] => do
      let index ← parseExpression indexExpr
      let value ← parseExpression valueExpr
      .ok (Statement.letStatement varName (some index) value)
    | _ => .error s!"Invalid let statement: {toString content}"

  parseIfStatement (content : List XMLNode) : Except String Statement :=
    match content with
    | [.element "keyword" [.text "if"],
       .element "symbol" [.text "("],
       condition,
       .element "symbol" [.text ")"],
       .element "symbol" [.text "{"],
       .element "statements" thenStatements,
       .element "symbol" [.text "}"]] => do
      let condExpr ← parseExpression condition
      let thenStmts ← thenStatements.mapM parseStatement
      .ok (.ifStatement condExpr thenStmts none)
    | [.element "keyword" [.text "if"],
       .element "symbol" [.text "("],
       condition,
       .element "symbol" [.text ")"],
       .element "symbol" [.text "{"],
       .element "statements" thenStatements,
       .element "symbol" [.text "}"],
       .element "keyword" [.text "else"],
       .element "symbol" [.text "{"],
       .element "statements" elseStatements,
       .element "symbol" [.text "}"]] => do
      let condExpr ← parseExpression condition
      let thenStmts ← thenStatements.mapM parseStatement
      let elseStmts ← elseStatements.mapM parseStatement
      .ok (Statement.ifStatement condExpr thenStmts (some elseStmts))
    | _ => .error s!"Invalid if statement: {toString content}"

  parseWhileStatement (content : List XMLNode) : Except String Statement :=
    match content with
    | [.element "keyword" [.text "while"],
       .element "symbol" [.text "("],
       condition,
       .element "symbol" [.text ")"],
       .element "symbol" [.text "{"],
       .element "statements" statements,
       .element "symbol" [.text "}"]] => do
      let condExpr ← parseExpression condition
      let stmts ← statements.mapM parseStatement
      .ok (Statement.whileStatement condExpr stmts)
    | _ => .error s!"Invalid while statement: {toString content}"

  parseDoStatement (content : List XMLNode) : Except String Statement :=
    match content with
    | [.element "keyword" [.text "do"],
       callExpr,
       .element "symbol" [.text ";"]] => do
      let expr ← parseExpression callExpr
      .ok (Statement.doStatement expr)
    | _ => .error s!"Invalid do statement: {toString content}"

  parseReturnStatement (content : List XMLNode) : Except String Statement :=
    match content with
    | [.element "keyword" [.text "return"], .element "symbol" [.text ";"]] =>
      .ok (Statement.returnStatement none)
    | [.element "keyword" [.text "return"], expr, .element "symbol" [.text ";"]] => do
      let expression ← parseExpression expr
      .ok (Statement.returnStatement (some expression))
    | _ => .error s!"Invalid return statement: {toString content}"

end Targil5
