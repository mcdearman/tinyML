module AST where

import Data.Text (Text)
import Spanned

newtype Root = Root [Spanned Decl] deriving (Show)

data Decl
  = Def (Spanned Text) (Spanned Expr)
  deriving (Show, Eq)

data Expr
  = Lit (Spanned Lit)
  | Var (Spanned Text)
  | App (Spanned Expr) (Spanned Expr)
  | Lam (Spanned Text) (Spanned Expr)
  | Let (Spanned Text) (Spanned Expr) (Spanned Expr)
  | Unit
  deriving (Show, Eq)

data Lit
  = Int Integer
  | Bool Bool
  | String Text
  deriving (Show, Eq)
