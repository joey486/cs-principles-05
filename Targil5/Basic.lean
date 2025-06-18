namespace JackCompiler

-- Basic types and utilities
def Identifier := String
def IntConstant := Int
def StringConstant := String

inductive JackType where
  | int
  | char
  | boolean
  | className (name : String)
  | void
deriving Repr, BEq

inductive VarKind where
  | static
  | field
  | argument
  | local
deriving Repr, BEq

structure VarInfo where
  name : String
  type : JackType
  kind : VarKind
  index : Nat
deriving Repr

-- Symbol table for managing variable scopes
structure SymbolTable where
  classScope : List VarInfo
  subroutineScope : List VarInfo
  staticCount : Nat
  fieldCount : Nat
  argCount : Nat
  localCount : Nat
deriving Repr

def SymbolTable.empty : SymbolTable :=
  { classScope := [], subroutineScope := [], staticCount := 0, fieldCount := 0, argCount := 0, localCount := 0 }

def SymbolTable.startSubroutine (st : SymbolTable) : SymbolTable :=
  { st with subroutineScope := [], argCount := 0, localCount := 0 }

def SymbolTable.define (st : SymbolTable) (name : String) (type : JackType) (kind : VarKind) : SymbolTable :=
  match kind with
  | .static =>
    let info := { name := name, type := type, kind := kind, index := st.staticCount }
    { st with classScope := info :: st.classScope, staticCount := st.staticCount + 1 }
  | .field =>
    let info := { name := name, type := type, kind := kind, index := st.fieldCount }
    { st with classScope := info :: st.classScope, fieldCount := st.fieldCount + 1 }
  | .argument =>
    let info := { name := name, type := type, kind := kind, index := st.argCount }
    { st with subroutineScope := info :: st.subroutineScope, argCount := st.argCount + 1 }
  | .local =>
    let info := { name := name, type := type, kind := kind, index := st.localCount }
    { st with subroutineScope := info :: st.subroutineScope, localCount := st.localCount + 1 }

def SymbolTable.lookup (st : SymbolTable) (name : String) : Option VarInfo :=
  (st.subroutineScope.find? (·.name == name)).orElse (fun _ => st.classScope.find? (·.name == name))

end JackCompiler
