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

data Operator m a -- N.B.
  = -- | Non-associative infix
    InfixN (m (a -> a -> a))
  | -- | Left-associative infix
    InfixL (m (a -> a -> a))
  | -- | Right-associative infix
    InfixR (m (a -> a -> a))
  | -- | Prefix
    Prefix (m (a -> a))
  | -- | Postfix
    Postfix (m (a -> a))

binary :: Text -> (Spanned Expr -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser (Spanned Expr)]]
operatorTable = undefined

expr :: Parser (Spanned Expr)
expr = try apply <|> atom
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
    lambda = withSpan $ do
      _ <- symbol "\\"
      args <- many ident
      _ <- symbol "->"
      Lam args <$> expr

    let' :: Parser (Spanned Expr)
    let' = withSpan $ do
      _ <- symbol "let"
      name <- ident
      _ <- symbol "="
      val <- expr
      _ <- symbol "in"
      Let name val <$> expr

    if' :: Parser (Spanned Expr)
    if' = withSpan $ do
      _ <- symbol "if"
      cond <- expr
      _ <- symbol "then"
      t <- expr
      _ <- symbol "else"
      If cond t <$> expr

    list :: Parser (Spanned Expr)
    list = withSpan $ do
      _ <- symbol "["
      elems <- sepEndBy expr (symbol ",")
      _ <- symbol "]"
      pure $ List elems

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
    apply = withSpan $ do
      f <- atom
      App f <$> some atom

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
