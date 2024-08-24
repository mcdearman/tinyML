module AST where

import Data.Text (Text)
import Spanned

newtype Root = Root [Spanned Decl] deriving (Show)

data Decl
  = DDef (Spanned Pattern) (Spanned Expr)
  | DFn (Spanned Text) [Spanned Pattern] (Spanned Expr)
  deriving (Show, Eq)

data Expr
  = ELit (Spanned Lit)
  | EVar (Spanned Text)
  | EApp (Spanned Expr) (Spanned Expr)
  | ELam [Spanned Pattern] (Spanned Expr)
  | ELet (Spanned Pattern) (Spanned Expr) (Spanned Expr)
  | ELetRec (Spanned Text) [Spanned Pattern] (Spanned Expr) (Spanned Expr)
  | EUnary (Spanned UnOp) (Spanned Expr)
  | EBinary (Spanned BinOp) (Spanned Expr) (Spanned Expr)
  | EIf (Spanned Expr) (Spanned Expr) (Spanned Expr)
  | EMatch (Spanned Expr) [(Spanned Pattern, Spanned Expr)]
  | EList [Spanned Expr]
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

data Pattern
  = PWildcard
  | PLit (Spanned Lit)
  | PVar (Spanned Text)
  | PPair (Spanned Text) [Spanned Pattern]
  | PList [Spanned Pattern]
  | PUnit
  deriving (Show, Eq)

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
