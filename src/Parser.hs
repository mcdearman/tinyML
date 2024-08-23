module Parser where

import AST
import Control.Applicative (empty, optional, (<|>))
import Data.Functor (($>))
import qualified Data.Functor
import Data.Maybe (fromMaybe)
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
    endBy,
    getOffset,
    many,
    manyTill,
    parse,
    satisfy,
    sepBy,
    sepEndBy,
    some,
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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

data Operator m a
  = InfixN (m (a -> a -> a))
  | InfixL (m (a -> a -> a))
  | InfixR (m (a -> a -> a))
  | Prefix (m (a -> a))
  | Postfix (m (a -> a))

binary :: Text -> (Spanned Expr -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser (Spanned Expr)]]
operatorTable = undefined

expr :: Parser (Spanned Expr)
expr = apply
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
    simple = choice [unit, litExpr, varExpr, parens expr]

    lambda :: Parser (Spanned Expr)
    lambda = withSpan $ Lam <$> (symbol "\\" *> some ident) <*> (symbol "->" *> expr)

    let' :: Parser (Spanned Expr)
    let' =
      withSpan $
        do
          Let <$> (symbol "let" *> ident)
          <*> (symbol "=" *> expr)
          <*> (symbol "in" *> expr)

    if' :: Parser (Spanned Expr)
    if' = withSpan $ do
      If
        <$> (symbol "if" *> expr)
        <*> (symbol "then" *> expr)
        <*> (symbol "else" *> expr)

    list :: Parser (Spanned Expr)
    list = withSpan $ List <$> brackets (expr `sepEndBy` symbol ",")

    atom :: Parser (Spanned Expr)
    atom =
      choice
        [ let',
          lambda,
          if',
          list,
          simple
        ]

    apply :: Parser (Spanned Expr)
    apply = do
      fargs <- some atom
      pure $ foldl1 (\f a -> Spanned (App f a) (span f <> span a)) fargs

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
