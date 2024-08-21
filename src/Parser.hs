module Parser where

import AST
import Control.Applicative (empty, (<|>))
import Data.Functor (($>))
import qualified Data.Functor
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

expr :: Parser (Spanned Expr)
expr = atom
  where
    unit :: Parser (Spanned Expr)
    unit = withSpan $ symbol "()" $> Unit

    litExpr = do
      l <- lit
      pure $ Spanned (Lit l) (span l)

    varExpr = do
      i <- ident
      pure $ Spanned (Var i) (span i)

    simple :: Parser (Spanned Expr)
    simple = choice [unit, litExpr, varExpr]

    lambda :: Parser (Spanned Expr)
    lambda = withSpan $ do
      _ <- symbol "\\"
      arg <- ident
      _ <- symbol "->"
      Lam arg <$> expr

    atom :: Parser (Spanned Expr)
    atom =
      choice
        [ simple,
          lambda,
          parens expr
        ]

decl :: Parser (Spanned Decl)
decl = withSpan $ Def <$> (symbol "def" *> ident) <*> (symbol "=" *> expr)

root :: Parser (Spanned Root)
root = withSpan $ Root <$> many decl

-- parse one decl or expr then wrap in a root
repl :: Parser (Spanned Root)
repl = sc *> (try declParser <|> exprParser)
  where
    declParser = do
      d <- decl
      let r = Root [d]
      pure $ Spanned r (span d)

    exprParser = do
      e <- expr
      let s = Gen $ span e
      let mainDecl = Spanned (Def (Spanned "main" s) e) s
      let r = Root [mainDecl]
      pure $ Spanned r s

parse :: Text -> Either (ParseErrorBundle Text Void) (Spanned Root)
parse = Text.Megaparsec.parse root ""

replParse :: Text -> Either (ParseErrorBundle Text Void) (Spanned Root)
replParse = Text.Megaparsec.parse repl ""
