module Parser where

import AST
import Control.Applicative (empty, (<|>))
import Data.Text (Text, pack)
import Data.Void
import Span
import Spanned
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    getOffset,
    many,
    manyTill,
    parse,
    satisfy,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    char',
    letterChar,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (span)

type Parser = Parsec Void Text

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  startPos <- getOffset
  result <- p
  Spanned result . SrcLoc startPos <$> getOffset

lexemeWithSpan :: Parser a -> Parser (Spanned a)
lexemeWithSpan p = withSpan p <* sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

symbol :: Text -> Parser (Spanned Text)
symbol p = withSpan (L.symbol sc p)

-- Token parsers
let' :: Parser (Spanned Text)
let' = symbol "let"

in' :: Parser (Spanned Text)
in' = symbol "in"

eq :: Parser (Spanned Text)
eq = symbol "="

octal :: Parser Integer
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Parser Integer
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Parser Integer
int = try octal <|> hexadecimal <|> L.decimal

signedInt :: Parser (Spanned Integer)
signedInt = lexemeWithSpan $ L.signed (notFollowedBy space1) int

real :: Parser (Spanned Double)
real = lexemeWithSpan $ L.signed (notFollowedBy space1) L.float

bool :: Parser (Spanned Bool)
bool = lexemeWithSpan $ choice [True <$ symbol "true", False <$ symbol "false"]

stringLiteral :: Parser (Spanned Text)
stringLiteral = lexemeWithSpan $ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"'))

lit :: Parser (Spanned Lit)
lit =
  choice
    [ fmap Int <$> signedInt,
      fmap Bool <$> bool,
      fmap String <$> stringLiteral
    ]

ident :: Parser (Spanned Text)
ident = lexemeWithSpan $ pack <$> ((:) <$> identStartChar <*> many identChar)
  where
    identStartChar = letterChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

atom :: Parser (Spanned Expr)
atom =
  choice
    [ litExpr,
      varExpr,
      parens expr
    ]
  where
    litExpr = do
      l <- lit
      pure $ Spanned (Lit l) (span l)
    varExpr = do
      i <- ident
      pure $ Spanned (Var i) (span i)

expr :: Parser (Spanned Expr)
expr = atom

def :: Parser (Spanned Def)
def = withSpan $ Def <$> (symbol "def" *> ident) <*> (symbol "=" *> expr)

repl :: Parser (Spanned Def)
repl =
  sc
    *> ( try def <|> do
           e <- expr
           let s = Gen $ span e
           pure $ Spanned (Def (Spanned "main" s) e) s
       )

parse :: Text -> Either (ParseErrorBundle Text Void) (Spanned Def)
parse = Text.Megaparsec.parse def ""

replParse :: Text -> Either (ParseErrorBundle Text Void) (Spanned Def)
replParse = Text.Megaparsec.parse repl ""
