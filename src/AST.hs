module AST where

import Data.Array (Array)
import Data.Text (Text)
import Spanned

newtype Root = Root [Spanned Decl] deriving (Show)

data Decl
  = DDef (Spanned Pattern) (Spanned Expr)
  | DFn Name [Spanned Pattern] (Spanned Expr)
  | DFnMatch Name [([Spanned Pattern], Spanned Expr)]
  | DRecordDef Name [(Name, Spanned TypeHint)]
  deriving (Show, Eq)

data Expr
  = ELit (Spanned Lit)
  | EVar Name
  | EApp (Spanned Expr) (Spanned Expr)
  | ELam [Spanned Pattern] (Spanned Expr)
  | ELet (Spanned Pattern) (Spanned Expr) (Spanned Expr)
  | ELetRec Name [Spanned Pattern] (Spanned Expr) (Spanned Expr)
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

data BinOp
  = BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BAnd
  | BOr
  | BEq
  | BNeq
  | BLt
  | BGt
  | BLeq
  | BGeq
  | BPair
  deriving (Show, Eq)

data TypeHint
  = THInt
  | THBool
  | THString
  | THVar Text
  | THIdent Name
  | THList (Spanned TypeHint)
  | THArray (Spanned TypeHint)
  | THTuple [Spanned TypeHint]
  | THArrow (Spanned TypeHint) (Spanned TypeHint)
  | THUnit
  deriving (Show, Eq)

data Pattern
  = PWildcard
  | PLit (Spanned Lit)
  | PVar Name
  | PPair (Spanned Pattern) (Spanned Pattern)
  | PList [Spanned Pattern]
  | PUnit
  deriving (Show, Eq)

type Name = Spanned Text

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
