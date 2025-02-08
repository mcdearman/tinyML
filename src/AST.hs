module AST where

import Common (Spanned)
import Data.Array (Array)
import Data.Text (Text)

type Prog = Spanned Module

data Module = Module Name [Spanned Decl] deriving (Show)

data Decl
  = DDef (Spanned Pattern) (Spanned Expr)
  | DFn Name [Spanned Pattern] (Spanned Expr)
  | DFnMatch Name (Maybe (Spanned TypeHint)) [([Spanned Pattern], Spanned Expr)]
  | DRecordDef Name [TyVar] [(Name, Spanned TypeHint)]
  | DData Name [TyVar] [(Name, [Spanned TypeHint])]
  | DTypeSyn Name [TyVar] (Spanned TypeHint)
  | DImport Path
  deriving (Show, Eq)

data Expr
  = ELit Lit
  | EVar Name
  | EApp (Spanned Expr) (Spanned Expr)
  | ELam [Spanned Pattern] (Spanned Expr)
  | ELet (Spanned Pattern) (Spanned Expr) (Spanned Expr)
  | EFn Name [Spanned Pattern] (Spanned Expr) (Spanned Expr)
  | EUnary (Spanned UnOp) (Spanned Expr)
  | EBinary (Spanned BinOp) (Spanned Expr) (Spanned Expr)
  | EIf (Spanned Expr) (Spanned Expr) (Spanned Expr)
  | EMatch (Spanned Expr) [(Spanned Pattern, Spanned Expr)]
  | EList [Spanned Expr]
  | EArray (Array Int (Spanned Expr))
  | ETuple [Spanned Expr]
  | ERecord (Maybe Name) [(Name, Spanned Expr)]
  | EUnit
  deriving (Show, Eq)

data UnOp
  = UNeg
  | UNot
  deriving (Show, Eq)

unOpName :: UnOp -> Text
unOpName UNeg = "__neg__"
unOpName UNot = "__not__"

data BinOp
  = BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BPow
  | BAnd
  | BOr
  | BEq
  | BNeq
  | BLt
  | BGt
  | BLeq
  | BGeq
  | BPair
  | BPipe
  deriving (Show, Eq)

binOpName :: BinOp -> Text
binOpName BAdd = "__add__"
binOpName BSub = "__sub__"
binOpName BMul = "__mul__"
binOpName BDiv = "__div__"
binOpName BMod = "__mod__"
binOpName BPow = "__pow__"
binOpName BAnd = "__and__"
binOpName BOr = "__or__"
binOpName BEq = "__eq__"
binOpName BNeq = "__neq__"
binOpName BLt = "__lt__"
binOpName BGt = "__gt__"
binOpName BLeq = "__lte__"
binOpName BGeq = "__gte__"
binOpName BPair = "__pair__"
binOpName BPipe = "__pipe__"

data TypeHint
  = THInt
  | THBool
  | THString
  | THVar TyVar
  | THIdent Name
  | THKind Name [Spanned TypeHint]
  | THList (Spanned TypeHint)
  | THArray (Spanned TypeHint)
  | THTuple [Spanned TypeHint]
  | THArrow (Spanned TypeHint) (Spanned TypeHint)
  | THRecord (Maybe Name) [(Name, Spanned TypeHint)]
  | THUnit
  deriving (Show, Eq)

data Pattern
  = PWildcard
  | PLit Lit
  | PVar Name
  | PPair (Spanned Pattern) (Spanned Pattern)
  | PList [Spanned Pattern]
  | PUnit
  deriving (Show, Eq)

type TyVar = Spanned Text

type Name = Spanned Text

type Path = Spanned [Name]

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
