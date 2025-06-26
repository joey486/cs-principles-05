import Targil5.symbol_table
import Targil5.xml_node
import Targil5.vm_writer
import Targil5.helpers

namespace VMGenerator

open VMGenerator.SymbolTable

mutual
partial def compileExpression (node : XMLNode) (st : SymbolTable) : VMWriter Unit :=
  match node with
  | XMLNode.elem "expression" children => compileExpressionList children st
  | _ => pure ()

partial def compileExpressionList (nodes : List XMLNode) (st : SymbolTable) : VMWriter Unit := do
  let rec compile (nodes : List XMLNode) (ops : List String) : VMWriter Unit := do
    match nodes with
    | [] =>
      -- Process operators in reverse order (right to left)
      for op in ops.reverse do
        arithmetic op
    | (XMLNode.elem "term" termChildren) :: rest => do
      -- Await compileTerm before continuing
      compileTerm termChildren st
      compile rest ops
    | (XMLNode.elem "symbol" [XMLNode.text op]) :: rest => do
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
  | [XMLNode.elem "integerConstant" [XMLNode.text n]] => do
    push "constant" (String.toNat! n)
  | [XMLNode.elem "stringConstant" [XMLNode.text s]] => do
    let len := s.length
    push "constant" len
    call "String.new" 1
    for c in s.toList do
      push "constant" c.toNat
      call "String.appendChar" 2
  | [XMLNode.elem "keyword" [XMLNode.text "true"]] => do
    push "constant" 0
    arithmetic "not"
  | [XMLNode.elem "keyword" [XMLNode.text "false"]] => do
    push "constant" 0
  | [XMLNode.elem "keyword" [XMLNode.text "null"]] => do
    push "constant" 0
  | [XMLNode.elem "keyword" [XMLNode.text "this"]] => do
    push "pointer" 0
  | [XMLNode.elem "identifier" [XMLNode.text name]] => do
    -- Simple variable
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
  -- Array access: identifier[expression]
  | (XMLNode.elem "identifier" [XMLNode.text name]) ::
    (XMLNode.elem "symbol" [XMLNode.text "["]) ::
    (XMLNode.elem "expression" exprChildren) ::
    (XMLNode.elem "symbol" [XMLNode.text "]"]) :: _ => do
    -- Push array base
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
    -- Compile index expression
    compileExpressionList exprChildren st
    -- Add base + index
    arithmetic "add"
    -- Pop to pointer 1 and push from that 0
    pop "pointer" 1
    push "that" 0
  -- Method call: identifier.method(args)
  | (XMLNode.elem "identifier" [XMLNode.text obj]) ::
    (XMLNode.elem "symbol" [XMLNode.text "."]) ::
    (XMLNode.elem "identifier" [XMLNode.text method]) ::
    (XMLNode.elem "symbol" [XMLNode.text "("]) ::
    (XMLNode.elem "expressionList" exprChildren) ::
    (XMLNode.elem "symbol" [XMLNode.text ")"]) :: _ => do
    let nArgs := countExpressions exprChildren
    compileExpressionList exprChildren st
    call s!"{obj}.{method}" nArgs
  | _ => do
    emit "// Complex term - TODO"
    push "constant" 0

end
