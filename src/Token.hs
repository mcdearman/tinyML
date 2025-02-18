module Token where

import Data.Text (Text)

data Token
  = TokEOF
  | TokWhitespace
  | TokComment
  | TokIdent Text
  | TokTypeIdent Text
  | TokTyVar Text
  | TokInt Int
  | TokReal Double
  | TokBool Bool
  | TokString Text
  | TokChar Char
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokHash
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokBackSlash
  | TokPercent
  | TokAssign
  | TokAnd
  | TokOr
  | TokBang
  | TokEq
  | TokNeq
  | TokLt
  | TokGt
  | TokLeq
  | TokGeq
  | TokPeriod
  | TokDoublePeriod
  | TokComma
  | TokColon
  | TokDoubleColon
  | TokSemiColon
  | TokArrow
  | TokFatArrow
  | TokBar
  | TokPipe
  | TokUnderscore
  | TokModule
  | TokImport
  | TokAs
  | TokPub
  | TokDef
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokMatch
  | TokWith
  | TokData
  | TokType
  | TokClass
  | TokInstance
  | TokDerive
  | TokDo
  | TokEnd
  deriving (Show, Eq, Ord)

pShowToken :: Token -> String
pShowToken TokEOF = "EOF"
pShowToken TokWhitespace = "Whitespace"
pShowToken TokComment = "Comment"
pShowToken (TokIdent x) = "Ident " ++ show x
pShowToken (TokTypeIdent x) = "TypeIdent " ++ show x
pShowToken (TokTyVar x) = "TyVar " ++ show x
pShowToken (TokInt x) = "Int" ++ show x
pShowToken (TokReal x) = "Real" ++ show x
pShowToken (TokBool x) = "Bool" ++ show x
pShowToken (TokString x) = "String" ++ show x
pShowToken (TokChar x) = "Char" ++ show x
pShowToken TokLParen = "LParen"
pShowToken TokRParen = "RParen"
pShowToken TokLBrace = "LBrace"
pShowToken TokRBrace = "RBrace"
pShowToken TokLBracket = "LBracket"
pShowToken TokRBracket = "RBracket"
pShowToken TokHash = "Hash"
pShowToken TokPlus = "Plus"
pShowToken TokMinus = "Minus"
pShowToken TokStar = "Star"
pShowToken TokSlash = "Slash"
pShowToken TokBackSlash = "BackSlash"
pShowToken TokPercent = "Percent"
pShowToken TokAssign = "Assign"
pShowToken TokAnd = "And"
pShowToken TokOr = "Or"
pShowToken TokBang = "Bang"
pShowToken TokEq = "Eq"
pShowToken TokNeq = "Neq"
pShowToken TokLt = "Lt"
pShowToken TokGt = "Gt"
pShowToken TokLeq = "Leq"
pShowToken TokGeq = "Geq"
pShowToken TokPeriod = "Period"
pShowToken TokDoublePeriod = "DoublePeriod"
pShowToken TokComma = "Comma"
pShowToken TokColon = "Colon"
pShowToken TokDoubleColon = "DoubleColon"
pShowToken TokSemiColon = "SemiColon"
pShowToken TokArrow = "Arrow"
pShowToken TokFatArrow = "FatArrow"
pShowToken TokBar = "Bar"
pShowToken TokPipe = "Pipe"
pShowToken TokUnderscore = "Underscore"
pShowToken TokModule = "Module"
pShowToken TokImport = "Import"
pShowToken TokAs = "As"
pShowToken TokPub = "Pub"
pShowToken TokDef = "Def"
pShowToken TokLet = "Let"
pShowToken TokIn = "In"
pShowToken TokIf = "If"
pShowToken TokThen = "Then"
pShowToken TokElse = "Else"
pShowToken TokMatch = "Match"
pShowToken TokWith = "With"
pShowToken TokData = "Data"
pShowToken TokType = "Type"
pShowToken TokClass = "Class"
pShowToken TokInstance = "Instance"
pShowToken TokDerive = "Derive"
pShowToken TokDo = "Do"
pShowToken TokEnd = "End"