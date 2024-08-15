module AST where

import Data.Text (Text)
import Spanned

data Def = Def (Spanned Text) (Spanned Expr) deriving (Show)

data Expr
  = Lit (Spanned Lit)
  | Var (Spanned Text)
  | App (Spanned Expr) (Spanned Expr)
  | Lam (Spanned Text) (Spanned Expr)
  | Let (Spanned Text) (Spanned Expr) (Spanned Expr)
  deriving (Show)

data Lit
  = Int Integer
  | Bool Bool
  | String String
  deriving (Show)