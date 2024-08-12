module AST.Expr where

import AST.Lit
import Data.Text
import Spanned

data Expr
  = Lit (Spanned Lit)
  | Var (Spanned Text)
  | App (Spanned Expr) (Spanned Expr)
  | Lam (Spanned Text) (Spanned Expr)
  | Let (Spanned Text) (Spanned Expr) (Spanned Expr)
  deriving (Show)