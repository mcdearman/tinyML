module Ty where

import Spanned
import Unique

data Ty
  = TInt
  | TBool
  | TChar
  | TString
  | TUnit
  | TVar TyVar
  | TArrow Ty Ty
  | TList Ty
  | TArray Ty
  | TTuple [Ty]
  | TRecord [(String, Ty)]
  | TCon String [Ty]
  deriving (Show, Eq, Ord)

newtype TyVar = TyVar Unique deriving (Show, Eq, Ord)

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)