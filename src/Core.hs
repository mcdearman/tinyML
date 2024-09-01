module Core where

import Data.Text
import Spanned
import TIR

data Expr
  = ELit Lit Ty
  | EVar Name

type Name = Spanned Text

data Lit
  = LInt Int
  | LBool Bool
  | LChar Char
  | LString String
  | LUnit
  deriving (Show, Eq)