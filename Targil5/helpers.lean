import Targil5.symbol_table
import Targil5.xml_node

namespace VMGenerator

open SymbolTable

-- ─── HELPER FUNCTIONS ─────────────────────────────────────────────────

def extractClassName (children : List XMLNode) : String :=
  let rec find (nodes : List XMLNode) : String :=
    match nodes with
    | (XMLNode.elem "keyword" [XMLNode.text "class"]) ::
      (XMLNode.elem "identifier" [XMLNode.text name]) :: _ => name
    | _ :: rest => find rest
    | [] => "Unknown"
  find children

def extractFunctionInfo (children : List XMLNode) : (String × String) :=
  let rec find (nodes : List XMLNode) : (String × String) :=
    match nodes with
    | (XMLNode.elem "keyword" [XMLNode.text kind]) ::
      (XMLNode.elem "keyword" [XMLNode.text _]) :: -- return type
      (XMLNode.elem "identifier" [XMLNode.text name]) :: _ => (kind, name)
    | _ :: rest => find rest
    | [] => ("function", "unknown")
  find children

-- Process parameter list and add to symbol table
partial def processParameterList (children : List XMLNode) (st : SymbolTable) : SymbolTable :=
  let rec process (nodes : List XMLNode) (symTable : SymbolTable) : SymbolTable :=
    match nodes with
    | (XMLNode.elem "keyword" [XMLNode.text typ]) ::
      (XMLNode.elem "identifier" [XMLNode.text name]) :: rest =>
      let newSt := SymbolTable.define symTable name typ "arg"
      match rest with
      | (XMLNode.elem "symbol" [XMLNode.text ","]) :: remaining => process remaining newSt
      | _ => process rest newSt
    | (XMLNode.elem "identifier" [XMLNode.text typ]) ::
      (XMLNode.elem "identifier" [XMLNode.text name]) :: rest =>
      let newSt := SymbolTable.define symTable name typ "arg"
      match rest with
      | (XMLNode.elem "symbol" [XMLNode.text ","]) :: remaining => process remaining newSt
      | _ => process rest newSt
    | _ :: rest => process rest symTable
    | [] => symTable

  process children st


-- Process variable declarations
def processVarDec (children : List XMLNode) (st : SymbolTable) : SymbolTable :=
  let rec extractVars (nodes : List XMLNode) (typ : String) (symTable : SymbolTable) : SymbolTable :=
    match nodes with
    | (XMLNode.elem "identifier" [XMLNode.text name]) :: (XMLNode.elem "symbol" [XMLNode.text ","]) :: rest =>
      let newSt := SymbolTable.define symTable name typ "var"
      extractVars rest typ newSt
    | (XMLNode.elem "identifier" [XMLNode.text name]) :: (XMLNode.elem "symbol" [XMLNode.text ";"]) :: _ =>
      SymbolTable.define symTable name typ "var"
    | (XMLNode.elem "identifier" [XMLNode.text name]) :: rest =>
      let newSt := SymbolTable.define symTable name typ "var"
      extractVars rest typ newSt
    | _ :: rest => extractVars rest typ symTable
    | [] => symTable

  let rec findType (nodes : List XMLNode) : String :=
    match nodes with
    | (XMLNode.elem "keyword" [XMLNode.text "var"]) :: (XMLNode.elem "keyword" [XMLNode.text typ]) :: _ => typ
    | (XMLNode.elem "keyword" [XMLNode.text "var"]) :: (XMLNode.elem "identifier" [XMLNode.text typ]) :: _ => typ
    | _ :: rest => findType rest
    | [] => "int"

  let typ := findType children
  extractVars children typ st


partial def countExpressions (children : List XMLNode) : Nat :=
    let rec count (nodes : List XMLNode) (current : Nat) : Nat :=
    match nodes with
    | [] => current
    | (XMLNode.elem "expression" _) :: rest =>
      count rest (current + 1)
    | _ :: rest => count rest current
  count children 0

end VMGenerator
