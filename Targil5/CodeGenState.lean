import Targil5.Basic
import Targil5.VMCommands

namespace JackCompiler

-- Code generation state
structure CodeGenState where
  labelCounter : Nat
  className : String
  symbolTable : SymbolTable
  vmCode : List VMCommand
deriving Repr

def CodeGenState.addCommand (state : CodeGenState) (cmd : VMCommand) : CodeGenState :=
  { state with vmCode := cmd :: state.vmCode }

def CodeGenState.addCommands (state : CodeGenState) (cmds : List VMCommand) : CodeGenState :=
  { state with vmCode := cmds ++ state.vmCode }

def CodeGenState.newLabel (state : CodeGenState) (prefixStr : String) : String × CodeGenState :=
  let label := s!"{prefixStr}_{state.labelCounter}"
  (label, { state with labelCounter := state.labelCounter + 1 })

end JackCompiler
