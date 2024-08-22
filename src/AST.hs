module AST where

import Data.Text (Text)
import Spanned

newtype Root = Root [Spanned Decl] deriving (Show)

data Decl
  = Def (Spanned Text) (Spanned Expr)
  | Fn (Spanned Text) [Spanned Text] (Spanned Expr)
  deriving (Show, Eq)

data Expr
  = Lit (Spanned Lit)
  | Var (Spanned Text)
  | App (Spanned Expr) [Spanned Expr]
  | Lam [Spanned Text] (Spanned Expr)
  | Let (Spanned Text) (Spanned Expr) (Spanned Expr)
  | Unary (Spanned UnOp) (Spanned Expr)
  | Binary (Spanned BinOp) (Spanned Expr) (Spanned Expr)
  | If (Spanned Expr) (Spanned Expr) (Spanned Expr)
  | List [Spanned Expr]
  | Unit
  deriving (Show, Eq)

data UnOp
  = Neg
  | Not
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Pair
  deriving (Show, Eq)

data Lit
  = Int Integer
  | Bool Bool
  | String Text
  deriving (Show, Eq)