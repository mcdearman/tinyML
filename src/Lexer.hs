module Lexer where

import Control.Applicative (empty, (<|>))
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
    letterChar,
    lowerChar,
    space1,
    string,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Token

type Lexer = Parsec Void Text

withSpan :: Lexer a -> Lexer (Spanned a)
withSpan p = do
  startPos <- getOffset
  result <- p
  Spanned result . SrcLoc startPos <$> getOffset

lexeme :: Lexer a -> Lexer a
lexeme p = p <* sc

lexemeWithSpan :: Lexer a -> Lexer (Spanned a)
lexemeWithSpan p = withSpan p <* sc

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "--") empty

octal :: Lexer Int
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Lexer Int
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Lexer Token
int = TInt <$> (try octal <|> hexadecimal <|> L.decimal)

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

token :: Lexer (Spanned Token)
token =
  lexemeWithSpan $
    choice
      [ TComment <$ string "--" <* manyTill L.charLiteral (char '\n'),
        int,
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
        TPercent <$ char '%',
        TAnd <$ string "&&",
        TOr <$ string "||",
        try (TNeq <$ string "!=") <|> TNot <$ char '!',
        try (TFatArrow <$ string "=>") <|> TEq <$ char '=',
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

lexMML :: Text -> Either (ParseErrorBundle Text Void) [Spanned Token]
lexMML = parse (many token <* eof) ""