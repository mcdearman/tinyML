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
  | TBackSlash
  | TPercent
  | TAssign
  | TAnd
  | TOr
  | TBang
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

pShowToken :: Token -> String
pShowToken TEOF = "EOF"
pShowToken TWhitespace = "Whitespace"
pShowToken TComment = "Comment"
pShowToken (TIdent x) = "Ident " ++ show x
pShowToken (TTypeIdent x) = "TypeIdent " ++ show x
pShowToken (TTyVar x) = "TyVar " ++ show x
pShowToken (TInt x) = "Int" ++ show x
pShowToken (TReal x) = "Real" ++ show x
pShowToken (TBool x) = "Bool" ++ show x
pShowToken (TString x) = "String" ++ show x
pShowToken (TChar x) = "Char" ++ show x
pShowToken TLParen = "LParen"
pShowToken TRParen = "RParen"
pShowToken TLBrace = "LBrace"
pShowToken TRBrace = "RBrace"
pShowToken TLBracket = "LBracket"
pShowToken TRBracket = "RBracket"
pShowToken THash = "Hash"
pShowToken TPlus = "Plus"
pShowToken TMinus = "Minus"
pShowToken TStar = "Star"
pShowToken TSlash = "Slash"
pShowToken TBackSlash = "BackSlash"
pShowToken TPercent = "Percent"
pShowToken TAssign = "Assign"
pShowToken TAnd = "And"
pShowToken TOr = "Or"
pShowToken TBang = "Bang"
pShowToken TEq = "Eq"
pShowToken TNeq = "Neq"
pShowToken TLt = "Lt"
pShowToken TGt = "Gt"
pShowToken TLeq = "Leq"
pShowToken TGeq = "Geq"
pShowToken TPeriod = "Period"
pShowToken TDoublePeriod = "DoublePeriod"
pShowToken TComma = "Comma"
pShowToken TColon = "Colon"
pShowToken TDoubleColon = "DoubleColon"
pShowToken TSemiColon = "SemiColon"
pShowToken TArrow = "Arrow"
pShowToken TFatArrow = "FatArrow"
pShowToken TBar = "Bar"
pShowToken TPipe = "Pipe"
pShowToken TUnderscore = "Underscore"
pShowToken TModule = "Module"
pShowToken TImport = "Import"
pShowToken TAs = "As"
pShowToken TPub = "Pub"
pShowToken TDef = "Def"
pShowToken TLet = "Let"
pShowToken TIn = "In"
pShowToken TIf = "If"
pShowToken TThen = "Then"
pShowToken TElse = "Else"
pShowToken TMatch = "Match"
pShowToken TWith = "With"
pShowToken TData = "Data"
pShowToken TType = "Type"
pShowToken TClass = "Class"
pShowToken TInstance = "Instance"
pShowToken TDerive = "Derive"
pShowToken TDo = "Do"
pShowToken TEnd = "End"