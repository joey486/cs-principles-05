namespace VMGenerator

-- Your VMWriter from above
abbrev VMWriter := StateT (List String) Id

def emit (cmd : String) : VMWriter Unit := modify (· ++ [cmd])
def push (segment : String) (index : Nat) : VMWriter Unit := emit s!"push {segment} {index}"
def pop (segment : String) (index : Nat) : VMWriter Unit := emit s!"pop {segment} {index}"
def arithmetic (cmd : String) : VMWriter Unit := emit cmd
def label (lbl : String) : VMWriter Unit := emit s!"label {lbl}"
def goto (lbl : String) : VMWriter Unit := emit s!"goto {lbl}"
def ifGoto (lbl : String) : VMWriter Unit := emit s!"if-goto {lbl}"
def call (name : String) (nArgs : Nat) : VMWriter Unit := emit s!"call {name} {nArgs}"
def function (name : String) (nLocals : Nat) : VMWriter Unit := emit s!"function {name} {nLocals}"
def ret : VMWriter Unit := emit "return"

-- Label generator helper
def freshLabel (m_prefix : String) (counter : Nat) : String × Nat :=
  (m_prefix ++ toString counter, counter + 1)

end VMGenerator
