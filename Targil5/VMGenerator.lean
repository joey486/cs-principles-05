import Targil5.Basic
import Targil5.AST
import Targil5.VMCommands
import Targil5.CodeGenState
import Targil5.ExpressionGenerator
import Targil5.StatementGenerator

namespace JackCompiler

-- Generate VM code for subroutines
def generateSubroutineCode (sub : SubroutineDeclaration) (state : CodeGenState) : Except String (List VMCommand × CodeGenState) := do
  let functionName := s!"{state.className}.{sub.name}"
  let numLocals := sub.localVars.foldl (fun acc decl => acc + decl.names.length) 0

  -- Set up symbol table for subroutine
  let symbolTable := state.symbolTable.startSubroutine
  let symbolTable := sub.parameters.foldl (fun st (type, name) =>
    st.define name type VarKind.argument) symbolTable
  let symbolTable := sub.localVars.foldl (fun st decl =>
    decl.names.foldl (fun st' name => st'.define name decl.type VarKind.local) st) symbolTable

  let state1 := { state with symbolTable := symbolTable }

  -- Generate function declaration
  let functionDecl := VMCommand.function functionName numLocals

  -- Generate method/constructor prologue
  let (prologueCommands, state2) := match sub.kind with
    | SubroutineKind.method =>
      ([VMCommand.push "argument" 0, VMCommand.pop "pointer" 0], state1)
    | SubroutineKind.constructor =>
      let fieldCount := state.symbolTable.fieldCount
      ([VMCommand.push "constant" fieldCount, VMCommand.call "Memory.alloc" 1, VMCommand.pop "pointer" 0], state1)
    | SubroutineKind.function => ([], state1)

  -- Generate body
  let (bodyCommands, state3) ← generateStatementsCode sub.body state2

  .ok ([functionDecl] ++ prologueCommands ++ bodyCommands, state3)

-- Generate VM code for entire class
def generateClassCode (cls : ClassDeclaration) : Except String (List VMCommand) := do
  let symbolTable := SymbolTable.empty
  let symbolTable := cls.staticVars.foldl (fun st decl =>
    decl.names.foldl (fun st' name => st'.define name decl.type VarKind.static) st) symbolTable
  let symbolTable := cls.fieldVars.foldl (fun st decl =>
    decl.names.foldl (fun st' name => st'.define name decl.type VarKind.field) st) symbolTable

  let state : CodeGenState := {
    className := cls.name,
    symbolTable := symbolTable,
    vmCode := [],
    labelCounter := 0
  }

  let (allCommands, _) ← cls.subroutines.foldlM (fun (commands, st) sub => do
    let (subCommands, newState) ← generateSubroutineCode sub st
    .ok (commands ++ subCommands, newState)
  ) ([], state)

  .ok allCommands

end JackCompiler
