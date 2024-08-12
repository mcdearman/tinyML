module Parser where

import qualified AST.Def as D
import qualified AST.Expr as E
import qualified AST.Lit as AL
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
  Spanned result . Span startPos <$> getOffset

lexemeWithSpan :: Parser a -> Parser (Spanned a)
lexemeWithSpan p = withSpan p <* sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

symbol :: Text -> Parser (Spanned Text)
symbol p = withSpan (L.symbol sc p)

-- Token parsers
-- let' :: Parser Text
-- let' = symbol "let"

-- in' :: Parser Text
-- in' = symbol "in"

-- eq :: Parser Text
-- eq = symbol "="

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

stringLiteral :: Parser (Spanned String)
stringLiteral = lexemeWithSpan $ char '\"' *> manyTill L.charLiteral (char '\"')

lit :: Parser (Spanned AL.Lit)
lit =
  choice
    [ fmap AL.Int <$> signedInt,
      fmap AL.Bool <$> bool,
      fmap AL.String <$> stringLiteral
    ]

ident :: Parser (Spanned Text)
ident = lexemeWithSpan $ pack <$> ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

atom :: Parser (Spanned E.Expr)
atom =
  choice
    [ litExpr,
      varExpr,
      parens expr
    ]
  where
    litExpr = do
      l <- lit
      return $ Spanned (E.Lit l) (span l)
    varExpr = do
      i <- ident
      return $ Spanned (E.Var i) (span i)

expr :: Parser (Spanned E.Expr)
expr = atom

def :: Parser (Spanned D.Def)
def = withSpan $ do
  i <- ident
  _ <- symbol "="
  D.Def i <$> expr

repl :: Parser (Spanned D.Def)
repl = def

parseDef :: Text -> Either (ParseErrorBundle Text Void) (Spanned D.Def)
parseDef = parse def ""
