module Lexer where

import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators (manyTill_)
import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Span
import Spanned
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    getOffset,
    getSourcePos,
    many,
    manyTill,
    parse,
    sepEndBy,
    sepEndBy1,
    some,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    char',
    lowerChar,
    space1,
    string,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Token
import TokenStream

type Lexer = Parsec Void Text

withPos :: Lexer a -> Lexer (WithPos a)
withPos p = do
  startPos <- getSourcePos
  startOffset <- getOffset
  result <- p
  endPos <- getSourcePos
  endOffset <- getOffset
  return $ WithPos startPos endPos (SrcLoc startOffset endOffset) (endOffset - startOffset) result

lexeme :: Lexer a -> Lexer a
lexeme p = p <* sc

lexemeWithPos :: Lexer a -> Lexer (WithPos a)
lexemeWithPos p = withPos p <* sc

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "--") empty

octal :: Lexer Int
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Lexer Int
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Lexer Token
int = TInt <$> (try octal <|> try hexadecimal <|> L.decimal)

real :: Lexer Token
real = TReal <$> L.float

bool :: Lexer Token
bool = TBool <$> choice [True <$ string "true", False <$ string "false"]

str :: Lexer Token
str = TString <$> (char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')))

ident :: Lexer Token
ident = try $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar)
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be an identifier"
    else return $ TIdent name
  where
    identStartChar = lowerChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

    keywords :: [Text]
    keywords =
      [ "module",
        "import",
        "as",
        "pub",
        "def",
        "let",
        "in",
        "if",
        "then",
        "else",
        "match",
        "with",
        "true",
        "false",
        "data",
        "type",
        "class",
        "instance",
        "derive",
        "do",
        "end"
      ]

typeIdent :: Lexer Token
typeIdent = TTypeIdent . pack <$> ((:) <$> upperChar <*> many alphaNumChar)

tyVar :: Lexer Token
tyVar = TTyVar . pack <$> ((:) <$> char '\'' <*> some lowerChar)

token :: Lexer (WithPos Token)
token =
  lexemeWithPos $
    choice
      [ TComment <$ string "--" <* manyTill_ L.charLiteral (char '\n'),
        try real <|> int,
        bool,
        str,
        typeIdent,
        tyVar,
        ident,
        TLParen <$ char '(',
        TRParen <$ char ')',
        TLBrace <$ char '{',
        TRBrace <$ char '}',
        TLBracket <$ char '[',
        TRBracket <$ char ']',
        THash <$ char '#',
        TPlus <$ char '+',
        try (TArrow <$ string "->") <|> TMinus <$ char '-',
        TStar <$ char '*',
        TSlash <$ char '/',
        TBackSlash <$ char '\\',
        TPercent <$ char '%',
        TAnd <$ string "&&",
        TOr <$ string "||",
        try (TNeq <$ string "!=") <|> TBang <$ char '!',
        try (TFatArrow <$ string "=>") <|> TEq <$ string "==" <|> TAssign <$ char '=',
        TLt <$ char '<',
        TGt <$ char '>',
        TLeq <$ string "<=",
        TGeq <$ string ">=",
        try (TDoublePeriod <$ string "..") <|> TPeriod <$ char '.',
        TComma <$ char ',',
        try (TDoubleColon <$ string "::") <|> TColon <$ char ':',
        TSemiColon <$ char ';',
        try (TPipe <$ string "|>") <|> TBar <$ char '|',
        TUnderscore <$ char '_',
        TModule <$ string "module",
        TImport <$ string "import",
        TAs <$ string "as",
        TPub <$ string "pub",
        TDef <$ string "def",
        TLet <$ string "let",
        TIn <$ string "in",
        TIf <$ string "if",
        TThen <$ string "then",
        TElse <$ string "else",
        TMatch <$ string "match",
        TWith <$ string "with",
        TData <$ string "data",
        TType <$ string "type",
        TClass <$ string "class",
        TInstance <$ string "instance",
        TDerive <$ string "derive",
        TDo <$ string "do",
        TEnd <$ string "end"
      ]

lexMML :: Text -> Either (ParseErrorBundle Text Void) TokenStream
lexMML src = TokenStream src <$> parse (many token) "" src