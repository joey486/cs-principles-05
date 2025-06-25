import Targil5.Basic
import Targil5.AST
import Targil5.VMCommands
import Targil5.CodeGenState
import Targil5.ExpressionGenerator

namespace JackCompiler

mutual

def generateStatementsCode (stmts : List Statement) (state : CodeGenState) : Except String (List VMCommand × CodeGenState) :=
  stmts.foldlM (fun (commands, st) stmt => do
    let (stmtCommands, newState) ← generateStatementCode stmt st
    .ok (commands ++ stmtCommands, newState)
  ) ([], state)


def generateStatementCode (stmt : Statement) (state : CodeGenState) : Except String (List VMCommand × CodeGenState) :=
  match stmt with
  | .letStatement varName arrayIndex value => do
    let (valueCommands, state1) ← generateExpressionCode value state
    match arrayIndex with
    | none =>
      match state1.symbolTable.lookup varName with
      | some varInfo =>
        let segment := match varInfo.kind with
          | .static => "static"
          | .field => "this"
          | .argument => "argument"
          | .local => "local"
        .ok (valueCommands ++ [.pop segment varInfo.index], state1)
      | none => .error s!"Undefined variable: {varName}"
    | some index => do
      let (indexCommands, state2) ← generateExpressionCode index state1
      match state2.symbolTable.lookup varName with
      | some varInfo =>
        let segment := match varInfo.kind with
          | .static => "static"
          | .field => "this"
          | .argument => "argument"
          | .local => "local"
        let commands := indexCommands ++ [
          .push segment varInfo.index,
          .add
        ] ++ valueCommands ++ [
          .pop "temp" 0,
          .pop "pointer" 1,
          .push "temp" 0,
          .pop "that" 0
        ]
        .ok (commands, state2)
      | none => .error s!"Undefined array: {varName}"
  | .ifStatement condition thenStmts elseStmts => do
    let (condCommands, state1) ← generateExpressionCode condition state
    let (elseLabel, state2) := state1.newLabel "IF_ELSE"
    let (endLabel, state3) := state2.newLabel "IF_END"
    let (thenCommands, state4) ← generateStatementsCode thenStmts state3
    let (elseCommands, state5) ← match elseStmts with
      | some stmts => generateStatementsCode stmts state4
      | none => .ok ([], state4)
    let commands := condCommands ++ [
      .not,
      .ifGoto elseLabel
    ] ++ thenCommands ++ [
      .goto endLabel,
      .label elseLabel
    ] ++ elseCommands ++ [
      .label endLabel
    ]
    .ok (commands, state5)
  | .whileStatement condition body => do
    let (startLabel, state1) := state.newLabel "WHILE_START"
    let (endLabel, state2) := state1.newLabel "WHILE_END"
    let (condCommands, state3) ← generateExpressionCode condition state2
    let (bodyCommands, state4) ← generateStatementsCode body state3
    let commands := [
      .label startLabel
    ] ++ condCommands ++ [
      .not,
      .ifGoto endLabel
    ] ++ bodyCommands ++ [
      .goto startLabel,
      .label endLabel
    ]
    .ok (commands, state4)
  | .doStatement call => do
    let (callCommands, state1) ← generateExpressionCode call state
    .ok (callCommands ++ [.pop "temp" 0], state1) -- Discard return value
  | .returnStatement value => do
    match value with
    | some expr => do
      let (exprCommands, state1) ← generateExpressionCode expr state
      .ok (exprCommands ++ [.return], state1)
    | none =>
      .ok ([.push "constant" 0, .return], state) -- Return 0 for void functions

where
  generateStatementsCode (stmts : List Statement) (state : CodeGenState) : Except String (List VMCommand × CodeGenState) :=
    stmts.foldlM (fun (commands, st) stmt => do
      let (stmtCommands, newState) ← generateStatementCode stmt st
      .ok (commands ++ stmtCommands, newState)
    ) ([], state)
end
