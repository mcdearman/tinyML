module TMC.Token (Token (..), pShowToken) where

import Data.Int (Int64)
import Data.Text (Text)

data Token
  = TokError
  | TokEOF
  | TokWhitespace
  | TokComment
  | TokUpperCaseIdent Text
  | TokLowerCaseIdent Text
  | TokInt Int64
  | TokBool Bool
  | TokString Text
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokBackSlash
  | TokPercent
  | TokEq
  | TokArrow
  | TokBar
  | TokUnderscore
  | TokDef
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokMatch
  | TokWith
  deriving (Show, Eq, Ord)

pShowToken :: Token -> String
pShowToken TokError = "Error"
pShowToken TokEOF = "EOF"
pShowToken TokWhitespace = "Whitespace"
pShowToken TokComment = "Comment"
pShowToken (TokUpperCaseIdent x) = "Ident " ++ show x
pShowToken (TokLowerCaseIdent x) = "Ident " ++ show x
pShowToken (TokInt x) = "Int" ++ show x
pShowToken (TokBool x) = "Bool" ++ show x
pShowToken (TokString x) = "String" ++ show x
pShowToken TokLParen = "LParen"
pShowToken TokRParen = "RParen"
pShowToken TokLBrace = "LBrace"
pShowToken TokRBrace = "RBrace"
pShowToken TokLBracket = "LBracket"
pShowToken TokRBracket = "RBracket"
pShowToken TokPlus = "Plus"
pShowToken TokMinus = "Minus"
pShowToken TokStar = "Star"
pShowToken TokSlash = "Slash"
pShowToken TokBackSlash = "BackSlash"
pShowToken TokPercent = "Percent"
pShowToken TokEq = "Assign"
pShowToken TokArrow = "Arrow"
pShowToken TokBar = "Bar"
pShowToken TokUnderscore = "Underscore"
pShowToken TokDef = "Def"
pShowToken TokLet = "Let"
pShowToken TokIn = "In"
pShowToken TokIf = "If"
pShowToken TokThen = "Then"
pShowToken TokElse = "Else"
pShowToken TokMatch = "Match"
pShowToken TokWith = "With"