module Core where

import Data.Text
import Spanned

data Expr
  = ELit Lit Ty
  | EVar Name

type Name = Spanned Text

data Ty
  = TInt
  | TBool
  | TChar
  | TString
  | TUnit
  | TVar String
  | TArrow Ty Ty
  | TList Ty
  | TArray Ty
  | TTuple [Ty]
  | TRecord [(String, Ty)]
  | TCon String [Ty]
  deriving (Show, Eq)
data Lit
  = LInt Int
  | LBool Bool
  | LChar Char
  | LString String
  | LUnit
  deriving (Show, Eq)
