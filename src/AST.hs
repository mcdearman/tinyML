module AST where

import Data.Text (Text)
import Spanned

newtype Root = Root [Spanned Decl] deriving (Show)

data Decl
  = DDef (Spanned Text) (Spanned Expr)
  | DFn (Spanned Text) [Spanned Text] (Spanned Expr)
  deriving (Show, Eq)
data Expr
  = ELit (Spanned Lit)
  | EVar (Spanned Text)
  | EApp (Spanned Expr) (Spanned Expr)
  | ELam [Spanned Text] (Spanned Expr)
  | ELet (Spanned Text) (Spanned Expr) (Spanned Expr)
  | ELetRec (Spanned Text) [Spanned Text] (Spanned Expr) (Spanned Expr)
  | EUnary (Spanned UnOp) (Spanned Expr)
  | EBinary (Spanned BinOp) (Spanned Expr) (Spanned Expr)
  | EIf (Spanned Expr) (Spanned Expr) (Spanned Expr)
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
  = PVar (Spanned Text)
  | PPair (Spanned Text) [Spanned Pattern]
  | PList [Spanned Pattern]
  | PUnit
  deriving (Show, Eq)

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
