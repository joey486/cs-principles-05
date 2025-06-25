import Targil5.Basic

namespace JackCompiler

-- AST nodes for Jack language
inductive Expression where
  | intConstant (value : Int)
  | stringConstant (value : String)
  | keywordConstant (value : String) -- true, false, null, this
  | varName (name : String)
  | arrayAccess (arrayName : String) (index : Expression)
  | subroutineCall (className : Option String) (subroutineName : String) (args : List Expression)
  | unaryOp (op : String) (expr : Expression)  -- -, ~
  | binaryOp (left : Expression) (op : String) (right : Expression) -- +, -, *, /, &, |, <, >, =
deriving Repr

inductive Statement where
  | letStatement (varName : String) (arrayIndex : Option Expression) (value : Expression)
  | ifStatement (condition : Expression) (thenStmts : List Statement) (elseStmts : Option (List Statement))
  | whileStatement (condition : Expression) (body : List Statement)
  | doStatement (call : Expression) -- Must be subroutineCall
  | returnStatement (value : Option Expression)
deriving Repr

inductive SubroutineKind where
  | constructor | function | method
deriving Repr, BEq

inductive SymbolKind where
  | static
  | field
  | argument
  | local
deriving Repr, BEq


structure VarDeclaration where
  type : JackType
  names : List String
deriving Repr

structure SubroutineDeclaration where
  kind : SubroutineKind
  returnType : JackType
  name : String
  parameters : List (JackType × String)
  localVars : List VarDeclaration
  body : List Statement
deriving Repr

structure ClassDeclaration where
  name : String
  staticVars : List VarDeclaration
  fieldVars : List VarDeclaration
  subroutines : List SubroutineDeclaration
deriving Repr

end JackCompiler
