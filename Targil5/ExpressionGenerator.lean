import Targil5.Basic
import Targil5.AST
import Targil5.VMCommands
import Targil5.CodeGenState

namespace JackCompiler

-- Generate VM code for expressions
def generateExpressionCode (expr : Expression) (state : CodeGenState) : Except String (List VMCommand × CodeGenState) :=
  match expr with
  | .intConstant value =>
    .ok ([.push "constant" value.natAbs], state)
  | .stringConstant value =>
    -- String handling requires OS library calls
    let commands := [
      .push "constant" value.length,
      .call "String.new" 1
    ] ++ (value.toList.map fun c => [.push "constant" c.toNat, .call "String.appendChar" 2]).flatten
    .ok (commands, state)
  | .keywordConstant "true" =>
    .ok ([.push "constant" 0, .not], state)
  | .keywordConstant "false" =>
    .ok ([.push "constant" 0], state)
  | .keywordConstant "null" =>
    .ok ([.push "constant" 0], state)
  | .keywordConstant "this" =>
    .ok ([.push "pointer" 0], state)
  | .varName name => do
    match state.symbolTable.lookup name with
    | some varInfo =>
      let segment := match varInfo.kind with
        | .static => "static"
        | .field => "this"
        | .argument => "argument"
        | .local => "local"
      .ok ([.push segment varInfo.index], state)
    | none => .error s!"Undefined variable: {name}"
  | .arrayAccess arrayName index => do
    let (indexCommands, state1) ← generateExpressionCode index state
    match state1.symbolTable.lookup arrayName with
    | some varInfo =>
      let segment := match varInfo.kind with
        | .static => "static"
        | .field => "this"
        | .argument => "argument"
        | .local => "local"
      let commands := indexCommands ++ [
        .push segment varInfo.index,
        .add,
        .pop "pointer" 1,
        .push "that" 0
      ]
      .ok (commands, state1)
    | none => .error s!"Undefined array: {arrayName}"
  | .unaryOp "-" expr => do
    let (exprCommands, state1) ← generateExpressionCode expr state
    .ok (exprCommands ++ [.neg], state1)
  | .unaryOp "~" expr => do
    let (exprCommands, state1) ← generateExpressionCode expr state
    .ok (exprCommands ++ [.not], state1)
  | .binaryOp left op right => do
    let (leftCommands, state1) ← generateExpressionCode left state
    let (rightCommands, state2) ← generateExpressionCode right state1
    let opCommand := match op with
      | "+" => [.add]
      | "-" => [.sub]
      | "*" => [.call "Math.multiply" 2]
      | "/" => [.call "Math.divide" 2]
      | "&" => [.and]
      | "|" => [.or]
      | "<" => [.lt]
      | ">" => [.gt]
      | "=" => [.eq]
      | _ => []
    .ok (leftCommands ++ rightCommands ++ opCommand, state2)
  | .subroutineCall className subroutineName args => do
    let (argCommands, state1) ← generateArgumentsCode args state
    let fullName := match className with
      | some cls => s!"{cls}.{subroutineName}"
      | none => s!"{state.className}.{subroutineName}"
    let numArgs := args.length
    .ok (argCommands ++ [.call fullName numArgs], state1)
  | _ => .error s!"Unsupported expression: {repr expr}"

where
  generateArgumentsCode (args : List Expression) (state : CodeGenState) : Except String (List VMCommand × CodeGenState) :=
    args.foldlM (fun (commands, st) arg => do
      let (argCommands, newState) ← generateExpressionCode arg st
      .ok (commands ++ argCommands, newState)
    ) ([], state)

end JackCompiler
