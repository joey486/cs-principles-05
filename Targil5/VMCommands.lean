namespace JackCompiler

-- VM Commands
inductive VMCommand where
  | push (segment : String) (index : Nat)
  | pop (segment : String) (index : Nat)
  | add | sub | neg
  | eq | gt | lt
  | and | or | not
  | label (name : String)
  | goto (label : String)
  | ifGoto (label : String)
  | call (functionName : String) (numArgs : Nat)
  | function (functionName : String) (numLocals : Nat)
  | return
deriving Repr

def VMCommand.toString (cmd : VMCommand) : String :=
  match cmd with
  | .push segment index => s!"push {segment} {index}"
  | .pop segment index => s!"pop {segment} {index}"
  | .add => "add"
  | .sub => "sub"
  | .neg => "neg"
  | .eq => "eq"
  | .gt => "gt"
  | .lt => "lt"
  | .and => "and"
  | .or => "or"
  | .not => "not"
  | .label name => s!"label {name}"
  | .goto lbl => s!"goto {lbl}"
  | .ifGoto lbl => s!"if-goto {lbl}"
  | .call functionName numArgs => s!"call {functionName} {numArgs}"
  | .function functionName numLocals => s!"function {functionName} {numLocals}"
  | .return => "return"

end JackCompiler
