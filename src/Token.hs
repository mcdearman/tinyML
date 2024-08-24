module Token where

import Data.Text (Text)

data Token
  = TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
  | TColon
  | TArrow
  | TEqual
  | TUnderscore
  | TDef
  | TLet
  | TIn
  | TIf
  | TThen
  | TElse
  | TIdent Text
  | TInt Int
  | TBool Bool
  | TString Text
  | TComment
  | TWhitespace
  | TEOF
  deriving (Show, Eq)