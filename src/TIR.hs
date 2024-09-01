module TIR where

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