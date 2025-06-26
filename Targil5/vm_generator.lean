import Targil5.xml_node
import Targil5.symbol_table
import Targil5.helpers
import Targil5.compile_expression
import Targil5.vm_writer
import Targil5.label_gen

namespace VMGenerator

open SymbolTable
open LabelGen

partial def vmGen (node : XMLNode) (symTable : SymbolTable) (className : String := "") (labelGen : LabelGen := LabelGen.new) : VMWriter (SymbolTable × LabelGen) := do
  match node with
  | XMLNode.elem "class" children => do
    let clsName := extractClassName children
    let mut st := symTable
    let mut lg := labelGen
    for c in children do
      match c with
      | XMLNode.elem "subroutineDec" _ =>
        let (newSt, newLg) ← vmGen c st clsName lg
        st := newSt
        lg := newLg
      | _ => pure ()
    return (st, lg)

  | XMLNode.elem "subroutineDec" children => do
    let st := SymbolTable.startSubroutine symTable
    let (kind, funcName) := extractFunctionInfo children
    let fullName := s!"{className}.{funcName}"

    -- Process parameters and vars first to build symbol table
    let mut stWithParams := st
    for c in children do
      match c with
      | XMLNode.elem "parameterList" paramChildren =>
        stWithParams := processParameterList paramChildren stWithParams
      | XMLNode.elem "subroutineBody" bodyChildren =>
        for bc in bodyChildren do
          match bc with
          | XMLNode.elem "varDec" varChildren =>
            stWithParams := processVarDec varChildren stWithParams
          | _ => pure ()
      | _ => pure ()

    let nLocals := SymbolTable.varCount stWithParams "var"

    function fullName nLocals

    if kind == "constructor" then do
      let nFields := SymbolTable.varCount symTable "field"
      push "constant" nFields
      call "Memory.alloc" 1
      pop "pointer" 0
    else if kind == "method" then do
      push "argument" 0
      pop "pointer" 0

    let mut lg := labelGen
    for c in children do
      match c with
      | XMLNode.elem "subroutineBody" bodyChildren =>
        for bc in bodyChildren do
          match bc with
          | XMLNode.elem "statements" _ =>
            let (_, newLg) ← vmGen bc stWithParams className lg
            lg := newLg
          | _ => pure ()
      | _ => pure ()

    return (stWithParams, lg)

  | XMLNode.elem "letStatement" children => do
    match children with
    | (XMLNode.elem "keyword" [XMLNode.text "let"]) ::
      (XMLNode.elem "identifier" [XMLNode.text name]) ::
      (XMLNode.elem "symbol" [XMLNode.text "="]) ::
      (XMLNode.elem "expression" exprChildren) ::
      (XMLNode.elem "symbol" [XMLNode.text ";"]) :: _ =>
      compileExpressionList exprChildren symTable
      match SymbolTable.kindOf? symTable name, SymbolTable.indexOf? symTable name with
      | some kind, some idx =>
        let segment := match kind with
          | "var" => "local"
          | "arg" => "argument"
          | "field" => "this"
          | "static" => "static"
          | _ => "temp"
        pop segment idx
      | _, _ =>
        emit s!"// Unknown variable: {name}"
        pop "temp" 0

    | (XMLNode.elem "keyword" [XMLNode.text "let"]) ::
      (XMLNode.elem "identifier" [XMLNode.text name]) ::
      (XMLNode.elem "symbol" [XMLNode.text "["]) ::
      (XMLNode.elem "expression" indexChildren) ::
      (XMLNode.elem "symbol" [XMLNode.text "]"]) ::
      (XMLNode.elem "symbol" [XMLNode.text "="]) ::
      (XMLNode.elem "expression" exprChildren) ::
      (XMLNode.elem "symbol" [XMLNode.text ";"]) :: _ =>
      match SymbolTable.kindOf? symTable name, SymbolTable.indexOf? symTable name with
      | some kind, some idx =>
        let segment := match kind with
          | "var" => "local"
          | "arg" => "argument"
          | "field" => "this"
          | "static" => "static"
          | _ => "temp"
        push segment idx
      | _, _ => push "constant" 0
      compileExpressionList indexChildren symTable
      arithmetic "add"
      compileExpressionList exprChildren symTable
      pop "temp" 0
      pop "pointer" 1
      push "temp" 0
      pop "that" 0

    | _ =>
      emit "// let statement - unsupported format"

    return (symTable, labelGen)

  | XMLNode.elem "doStatement" children => do
    match children with
    | (XMLNode.elem "keyword" [XMLNode.text "do"]) ::
      (XMLNode.elem "identifier" [XMLNode.text obj]) ::
      (XMLNode.elem "symbol" [XMLNode.text "."]) ::
      (XMLNode.elem "identifier" [XMLNode.text method]) ::
      (XMLNode.elem "symbol" [XMLNode.text "("]) ::
      (XMLNode.elem "expressionList" exprChildren) ::
      (XMLNode.elem "symbol" [XMLNode.text ")"]) ::
      (XMLNode.elem "symbol" [XMLNode.text ";"]) :: _ =>
      let nArgs := countExpressions exprChildren
      compileExpressionList exprChildren symTable
      call s!"{obj}.{method}" nArgs
      pop "temp" 0
    | _ =>
      emit "// do statement - unsupported format"
      pop "temp" 0

    return (symTable, labelGen)

  | XMLNode.elem "returnStatement" children => do
    match children with
    | (XMLNode.elem "keyword" [XMLNode.text "return"]) ::
      (XMLNode.elem "symbol" [XMLNode.text ";"]) :: _ =>
      push "constant" 0
      ret
    | (XMLNode.elem "keyword" [XMLNode.text "return"]) ::
      (XMLNode.elem "expression" exprChildren) ::
      (XMLNode.elem "symbol" [XMLNode.text ";"]) :: _ =>
      compileExpressionList exprChildren symTable
      ret
    | _ =>
      emit "// return statement - unsupported format"
      ret

    return (symTable, labelGen)

  | XMLNode.elem "ifStatement" children => do
    let (ifLabel, lg1) := LabelGen.next labelGen "IF_TRUE"
    let (elseLabel, lg2) := LabelGen.next lg1 "IF_FALSE"
    let (endLabel, lg3) := LabelGen.next lg2 "IF_END"

    let rec processIf (nodes : List XMLNode) (foundCond foundIf foundElse : Bool) : VMWriter Unit := do
      match nodes with
      | [] => pure ()
      | (XMLNode.elem "expression" exprChildren) :: rest =>
        if !foundCond then do
          compileExpressionList exprChildren symTable
          ifGoto ifLabel
          goto elseLabel
          processIf rest true foundIf foundElse
        else
          processIf rest foundCond foundIf foundElse
      | (XMLNode.elem "statements" stmtChildren) :: rest =>
        if !foundIf then do
          label ifLabel
          for stmt in stmtChildren do
            let _ ← vmGen stmt symTable className lg3
            pure ()
          if !foundElse then goto endLabel
          processIf rest foundCond true foundElse
        else if !foundElse then do
          label elseLabel
          for stmt in stmtChildren do
            let _ ← vmGen stmt symTable className lg3
            pure ()
          processIf rest foundCond foundIf true
        else
          processIf rest foundCond foundIf foundElse
      | _ :: rest => processIf rest foundCond foundIf foundElse

    processIf children false false false
    label endLabel
    return (symTable, lg3)

  | XMLNode.elem "whileStatement" children => do
    let (loopLabel, lg1) := LabelGen.next labelGen "WHILE_EXP"
    let (endLabel, lg2) := LabelGen.next lg1 "WHILE_END"

    label loopLabel

    let rec processWhile (nodes : List XMLNode) (foundCond : Bool) : VMWriter Unit := do
      match nodes with
      | [] => pure ()
      | (XMLNode.elem "expression" exprChildren) :: rest =>
        if !foundCond then do
          compileExpressionList exprChildren symTable
          arithmetic "not"
          ifGoto endLabel
          processWhile rest true
        else
          processWhile rest foundCond
      | (XMLNode.elem "statements" stmtChildren) :: rest => do
        for stmt in stmtChildren do
          let _ ← vmGen stmt symTable className lg2
          pure ()
        goto loopLabel
        processWhile rest foundCond
      | _ :: rest => processWhile rest foundCond

    processWhile children false
    label endLabel
    return (symTable, lg2)

  | XMLNode.elem "statements" children => do
    let mut st := symTable
    let mut lg := labelGen
    for c in children do
      let (newSt, newLg) ← vmGen c st className lg
      st := newSt
      lg := newLg
    return (st, lg)

  | XMLNode.elem _ children => do
    let mut st := symTable
    let mut lg := labelGen
    for c in children do
      let (newSt, newLg) ← vmGen c st className lg
      st := newSt
      lg := newLg
    return (st, lg)

  | XMLNode.text _ =>
    return (symTable, labelGen)

end VMGenerator
