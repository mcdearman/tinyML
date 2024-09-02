module Token where

import Data.Text (Text)

data Token
  = TEOF
  | TWhitespace
  | TComment
  | TIdent Text
  | TTypeIdent Text
  | TTyVar Text
  | TInt Int
  | TReal Double
  | TBool Bool
  | TString Text
  | TChar Char
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | THash
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TPercent
  | TAnd
  | TOr
  | TNot
  | TEq
  | TNeq
  | TLt
  | TGt
  | TLeq
  | TGeq
  | TPeriod
  | TDoublePeriod
  | TComma
  | TColon
  | TDoubleColon
  | TSemiColon
  | TArrow
  | TFatArrow
  | TBar
  | TPipe
  | TUnderscore
  | TModule
  | TImport
  | TAs
  | TPub
  | TDef
  | TLet
  | TIn
  | TIf
  | TThen
  | TElse
  | TMatch
  | TWith
  | TData
  | TType
  | TClass
  | TInstance
  | TDerive
  | TDo
  | TEnd
  deriving (Show, Eq, Ord)
