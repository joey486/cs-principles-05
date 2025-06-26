import Targil5.symbol_table
import Targil5.xml_node
import Targil5.vm_writer
import Targil5.helpers

namespace VMGenerator

variable (labelCounter : Nat := 0)

def freshLabelId : VMWriter Nat := do
  let current := labelCounter
  -- labelCounter := labelCounter + 1  -- This would need to be handled differently
  return current

-- XMLNode helpers:
namespace XMLNode

def tag? : XMLNode → Option String
  | XMLNode.elem tag _ => some tag
  | _ => none

partial def find? (node : XMLNode) (p : XMLNode → Bool) : Option XMLNode :=
  match node with
  | XMLNode.elem _ children => children.find? p
  | _ => none

end XMLNode

mutual

partial def compileClass (node : XMLNode) : VMWriter Unit := do
  let className :=
    match node with
    | XMLNode.elem "class" (_ :: XMLNode.elem "identifier" [XMLNode.text name] :: _) => name
    | _ => "Unknown"
  let subroutines :=
    match node with
    | XMLNode.elem "class" children =>
      children.filterMap (fun c => match c with
        | XMLNode.elem "subroutineDec" _ => some c
        | _ => none)
    | _ => []
  for sub in subroutines do
    compileSubroutine className sub

partial def compileSubroutine (className : String) (node : XMLNode) : VMWriter Unit := do
  let (subKind, subName, varCount) :=
    match node with
    | XMLNode.elem "subroutineDec" (XMLNode.elem "keyword" [XMLNode.text kind] ::
                                  _ ::
                                  XMLNode.elem "identifier" [XMLNode.text name] ::
                                  _ :: body :: _) =>
      let vars := collectVarDecs body
      (kind, name, vars)
    | _ => ("function", "unknown", 0)
  emit s!"function {className}.{subName} {varCount}"
  match node with
  | XMLNode.elem "subroutineDec" children =>
    match children with
    | _ :: _ :: _ :: _ :: body :: _ => compileStatements body
    | _ => pure ()
  | _ => pure ()

partial def collectVarDecs (node : XMLNode) : Nat :=
  match node with
  | XMLNode.elem "subroutineBody" children =>
    let vars := children.filterMap (fun c =>
      match c with
      | XMLNode.elem "varDec" _ => some c
      | _ => none)
    vars.foldl (fun acc v =>
      match v with
      | XMLNode.elem "varDec" (_ :: _ :: ids) =>
        acc + ids.length
      | _ => acc) 0
  | _ => 0

partial def compileStatements (node : XMLNode) : VMWriter Unit := do
  let stmtsNode :=
    match node with
    | XMLNode.elem "subroutineBody" children =>
      children.find? (fun c =>
        match c with
        | XMLNode.elem "statements" _ => true
        | _ => false)
      |>.getD (XMLNode.elem "statements" [])
    | _ => XMLNode.elem "statements" []
  compileStatementList stmtsNode

partial def compileStatementList (node : XMLNode) : VMWriter Unit := do
  let statements :=
    match node with
    | XMLNode.elem "statements" children => children
    | _ => []
  for stmt in statements do
    match stmt with
    | XMLNode.elem "letStatement" _ => compileLetStatement stmt
    | XMLNode.elem "doStatement" _ => compileDoStatement stmt
    | XMLNode.elem "returnStatement" _ => compileReturnStatement stmt
    | XMLNode.elem "whileStatement" _ => compileWhileStatement stmt
    | XMLNode.elem "ifStatement" _ => compileIfStatement stmt
    | _ => emit s!"// Unsupported statement"

partial def compileLetStatement (node : XMLNode) : VMWriter Unit := do
  let rec findIdentAndExpr (children : List XMLNode) : Option (String × Option (List XMLNode) × List XMLNode) :=
    match children with
    | (XMLNode.elem "identifier" [XMLNode.text name]) :: rest =>
      match rest with
      | (XMLNode.elem "symbol" [XMLNode.text "["]) :: exprChildren :: (XMLNode.elem "symbol" [XMLNode.text "]"]) :: r2 =>
        match r2 with
        | (XMLNode.elem "symbol" [XMLNode.text "="]) :: expr2 :: _ => some (name, some [exprChildren], [expr2])
        | _ => none
      | (XMLNode.elem "symbol" [XMLNode.text "="]) :: expr2 :: _ => some (name, none, [expr2])
      | _ => none
    | _ :: rest => findIdentAndExpr rest
    | [] => none

  match node with
  | XMLNode.elem "letStatement" children =>
    match findIdentAndExpr children with
    | some (varName, maybeIndexExpr, exprNodes) => do
      compileExpressionList exprNodes SymbolTable.empty
      match maybeIndexExpr with
      | some indexExpr => do
        match SymbolTable.kindOf? SymbolTable.empty varName, SymbolTable.indexOf? SymbolTable.empty varName with
        | some kind, some idx =>
          let segment := match kind with
            | "var" => "local"
            | "arg" => "argument"
            | "field" => "this"
            | "static" => "static"
            | _ => "temp"
          push segment idx
        | _, _ => push "constant" 0
        compileExpressionList indexExpr SymbolTable.empty
        arithmetic "add"
        pop "pointer" 1
        pop "that" 0
      | none => do
        match SymbolTable.kindOf? SymbolTable.empty varName, SymbolTable.indexOf? SymbolTable.empty varName with
        | some kind, some idx =>
          let segment := match kind with
            | "var" => "local"
            | "arg" => "argument"
            | "field" => "this"
            | "static" => "static"
            | _ => "temp"
          pop segment idx
        | _, _ => pop "temp" 0
    | none => emit "// let statement parse failed"
  | _ => pure ()

partial def compileDoStatement (node : XMLNode) : VMWriter Unit := do
  let rec findSubroutineCall (children : List XMLNode) : Option (List XMLNode) :=
    match children with
    | (XMLNode.elem "identifier" _ :: XMLNode.elem "symbol" [XMLNode.text "("] :: XMLNode.elem "expressionList" _ :: XMLNode.elem "symbol" [XMLNode.text ")"] :: _) => some children
    | (XMLNode.elem "identifier" _ :: XMLNode.elem "symbol" [XMLNode.text "."] :: XMLNode.elem "identifier" _ :: XMLNode.elem "symbol" [XMLNode.text "("] :: XMLNode.elem "expressionList" _ :: XMLNode.elem "symbol" [XMLNode.text ")"] :: _) => some children
    | _ :: rest => findSubroutineCall rest
    | [] => none
  match node with
  | XMLNode.elem "doStatement" children =>
    match findSubroutineCall children with
    | some callNodes => do
      compileTerm callNodes SymbolTable.empty
      pop "temp" 0 -- discard return value
    | none => emit "// do statement parse failed"
  | _ => pure ()

partial def compileReturnStatement (node : XMLNode) : VMWriter Unit := do
  match node with
  | XMLNode.elem "returnStatement" (_ :: rest) =>
    if rest.isEmpty then
      push "constant" 0
    else
      compileExpressionList rest SymbolTable.empty
  | _ => pure ()

partial def compileWhileStatement (node : XMLNode) : VMWriter Unit := do
  let labelId ← freshLabelId
  let expChildren :=
    match node with
    | XMLNode.elem "whileStatement" children =>
      children.find? (fun c => c.tag? = some "expression") |>.getD (XMLNode.elem "expression" [])
    | _ => XMLNode.elem "expression" []
  let stmtsNode :=
    match node with
    | XMLNode.elem "whileStatement" children =>
      children.find? (fun c => c.tag? = some "statements") |>.getD (XMLNode.elem "statements" [])
    | _ => XMLNode.elem "statements" []

  emit s!"label WHILE_EXP{labelId}"
  compileExpression expChildren SymbolTable.empty
  arithmetic "not"
  emit s!"if-goto WHILE_END{labelId}"
  compileStatementList stmtsNode
  emit s!"goto WHILE_EXP{labelId}"
  emit s!"label WHILE_END{labelId}"

partial def compileIfStatement (node : XMLNode) : VMWriter Unit := do
  let labelId ← freshLabelId
  let expChildren :=
    match node with
    | XMLNode.elem "ifStatement" children =>
      children.find? (fun c => c.tag? = some "expression") |>.getD (XMLNode.elem "expression" [])
    | _ => XMLNode.elem "expression" []
  let thenStmts :=
    match node with
    | XMLNode.elem "ifStatement" children =>
      children.find? (fun c => c.tag? = some "statements") |>.getD (XMLNode.elem "statements" [])
    | _ => XMLNode.elem "statements" []
  let elseStmts :=
    match node with
    | XMLNode.elem "ifStatement" children =>
      children.find? (fun c => c.tag? = some "else") >>= fun elseNode =>
      elseNode.find? (fun c => c.tag? = some "statements")
    | _ => none

  compileExpression expChildren SymbolTable.empty
  emit s!"if-goto IF_TRUE{labelId}"
  emit s!"goto IF_FALSE{labelId}"
  emit s!"label IF_TRUE{labelId}"
  compileStatementList thenStmts
  emit s!"goto IF_END{labelId}"
  emit s!"label IF_FALSE{labelId}"
  match elseStmts with
  | some elseS => compileStatementList elseS
  | none => pure ()
  emit s!"label IF_END{labelId}"

partial def compileExpression (node : XMLNode) (st : SymbolTable) : VMWriter Unit :=
  match node with
  | XMLNode.elem "expression" children => compileExpressionList children st
  | _ => pure ()

partial def compileExpressionList (nodes : List XMLNode) (st : SymbolTable) : VMWriter Unit := do
  let rec compile (nodes : List XMLNode) (ops : List String) : VMWriter Unit := do
    match nodes with
    | [] =>
      for op in ops.reverse do arithmetic op
    | (XMLNode.elem "term" termChildren) :: rest =>
      compileTerm termChildren st
      compile rest ops
    | (XMLNode.elem "symbol" [XMLNode.text op]) :: rest =>
      let vmOp := match op with
        | "+" => "add"
        | "-" => "sub"
        | "*" => "call Math.multiply 2"
        | "/" => "call Math.divide 2"
        | "&lt;" => "lt"
        | "&gt;" => "gt"
        | "=" => "eq"
        | "&amp;" => "and"
        | "|" => "or"
        | _ => "nop"
      compile rest (vmOp :: ops)
    | _ :: rest => compile rest ops
  compile nodes []

partial def compileTerm (children : List XMLNode) (st : SymbolTable) : VMWriter Unit :=
  match children with
  | [XMLNode.elem "integerConstant" [XMLNode.text n]] => push "constant" (String.toNat! n)
  | [XMLNode.elem "stringConstant" [XMLNode.text s]] => do
      push "constant" s.length
      call "String.new" 1
      for c in s.toList do
        push "constant" (Char.toNat c)
        call "String.appendChar" 2
  | [XMLNode.elem "keyword" [XMLNode.text "true"]] => do
      push "constant" 0
      arithmetic "not"
  | [XMLNode.elem "keyword" [XMLNode.text "false"]]
  | [XMLNode.elem "keyword" [XMLNode.text "null"]] =>
      push "constant" 0
  | [XMLNode.elem "keyword" [XMLNode.text "this"]] =>
      push "pointer" 0
  | [XMLNode.elem "identifier" [XMLNode.text name]] => do
      match SymbolTable.kindOf? st name, SymbolTable.indexOf? st name with
      | some kind, some idx =>
          let segment := match kind with
            | "var" => "local"
            | "arg" => "argument"
            | "field" => "this"
            | "static" => "static"
            | _ => "temp"
          push segment idx
      | _, _ => do
          emit s!"// Unknown variable: {name}"
          push "constant" 0
  | (XMLNode.elem "identifier" [XMLNode.text name]) ::
    (XMLNode.elem "symbol" [XMLNode.text "["]) ::
    (XMLNode.elem "expression" exprChildren) ::
    (XMLNode.elem "symbol" [XMLNode.text "]"]) :: _ => do
      match SymbolTable.kindOf? st name, SymbolTable.indexOf? st name with
      | some kind, some idx =>
          let segment := match kind with
            | "var" => "local"
            | "arg" => "argument"
            | "field" => "this"
            | "static" => "static"
            | _ => "temp"
          push segment idx
      | _, _ => push "constant" 0
      compileExpressionList exprChildren st
      arithmetic "add"
      pop "pointer" 1
      push "that" 0
  | (XMLNode.elem "identifier" [XMLNode.text method]) ::
    (XMLNode.elem "symbol" [XMLNode.text "("]) ::
    (XMLNode.elem "expressionList" exprChildren) ::
    (XMLNode.elem "symbol" [XMLNode.text ")"]) :: _ => do
      push "pointer" 0
      let nArgs := 1 + countExpressions exprChildren
      compileExpressionList exprChildren st
      call method nArgs
  | (XMLNode.elem "identifier" [XMLNode.text obj]) ::
    (XMLNode.elem "symbol" [XMLNode.text "."]) ::
    (XMLNode.elem "identifier" [XMLNode.text method]) ::
    (XMLNode.elem "symbol" [XMLNode.text "("]) ::
    (XMLNode.elem "expressionList" exprChildren) ::
    (XMLNode.elem "symbol" [XMLNode.text ")"]) :: _ => do
      match SymbolTable.kindOf? st obj, SymbolTable.indexOf? st obj with
      | some kind, some idx =>
          let segment := match kind with
            | "var" => "local"
            | "arg" => "argument"
            | "field" => "this"
            | "static" => "static"
            | _ => "temp"
          push segment idx
          let nArgs := 1 + countExpressions  exprChildren
          compileExpressionList exprChildren st
          call s!"{obj}.{method}" nArgs
      | _, _ =>
          let nArgs := countExpressions exprChildren
          compileExpressionList exprChildren st
          call s!"{obj}.{method}" nArgs
  | (XMLNode.elem "symbol" [XMLNode.text "-"]) :: rest => do
      compileTerm rest st
      arithmetic "neg"
  | (XMLNode.elem "symbol" [XMLNode.text "~"]) :: rest => do
      compileTerm rest st
      arithmetic "not"
  | (XMLNode.elem "symbol" [XMLNode.text "("]) ::
    (XMLNode.elem "expression" exprChildren) ::
    (XMLNode.elem "symbol" [XMLNode.text ")"]) :: _ =>
      compileExpressionList exprChildren st
  | _ => emit "// Unhandled term"

end
