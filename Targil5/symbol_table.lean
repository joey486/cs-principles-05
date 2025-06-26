import Std.Data.HashMap
open Std

namespace VMGenerator

structure SymbolInfo where
  typ : String
  kind : String -- "static", "field", "arg", "var"
  index : Nat
  deriving Repr

structure SymbolTable where
  classScope : HashMap String SymbolInfo
  subroutineScope : HashMap String SymbolInfo
  counts : HashMap String Nat
  deriving Repr

namespace SymbolTable

def HashMap.findD {α β} [BEq α] [Hashable α] (m : HashMap α β) (key : α) (default : β) : β :=
  match m.get? key with
  | some v => v
  | none => default

abbrev Kind := String
abbrev Scope := SymbolTable

def empty : SymbolTable :=
  let countsInit := HashMap.emptyWithCapacity
    |>.insert "static" 0
    |>.insert "field" 0
    |>.insert "arg" 0
    |>.insert "var" 0
  { classScope := HashMap.emptyWithCapacity,
    subroutineScope := HashMap.emptyWithCapacity,
    counts := countsInit }

def startSubroutine (st : SymbolTable) : SymbolTable :=
  let countsReset := st.counts.insert "arg" 0 |>.insert "var" 0
  { st with
    subroutineScope := HashMap.emptyWithCapacity,
    counts := countsReset }

def define (st : SymbolTable) (name typ kind : String) : SymbolTable :=
  let idx := HashMap.findD st.counts kind 0
  let info := SymbolInfo.mk typ kind idx
  let updatedCounts := st.counts.insert kind (idx + 1)
  match kind with
  | "static" | "field" =>
    { st with
      classScope := st.classScope.insert name info,
      counts := updatedCounts }
  | "arg" | "var" =>
    { st with
      subroutineScope := st.subroutineScope.insert name info,
      counts := updatedCounts }
  | _ => st

def varCount (st : SymbolTable) (kind : String) : Nat :=
  HashMap.findD st.counts kind 0

def kindOf? (st : SymbolTable) (name : String) : Option String :=
  match st.subroutineScope.get? name with
  | some info => some info.kind
  | none => st.classScope.get? name |>.map (·.kind)

def typeOf? (st : SymbolTable) (name : String) : Option String :=
  match st.subroutineScope.get? name with
  | some info => some info.typ
  | none => st.classScope.get? name |>.map (·.typ)

def indexOf? (st : SymbolTable) (name : String) : Option Nat :=
  match st.subroutineScope.get? name with
  | some info => some info.index
  | none => st.classScope.get? name |>.map (·.index)

end SymbolTable
