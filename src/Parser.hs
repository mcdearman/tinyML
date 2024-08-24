module Parser where

import AST
import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Data.Void
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
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
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
    [ fmap LInt <$> signedInt,
      fmap LBool <$> bool,
      fmap LString <$> stringLiteral
    ]

ident :: Parser (Spanned Text)
ident = lexemeWithSpan $ try $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar)
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be an identifier"
    else return name
  where
    identStartChar = letterChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

    keywords :: [Text]
    keywords = ["def", "let", "in", "if", "then", "else"]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

binary :: Text -> (Span -> Spanned Expr -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
binary name f = InfixL (f . span <$> symbol name)

prefix, postfix :: Text -> (Span -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
prefix name f = Prefix (f . span <$> symbol name)
postfix name f = Postfix (f . span <$> symbol name)

operatorTable :: [[Operator Parser (Spanned Expr)]]
operatorTable =
  [ [ prefix "-" (\s e -> Spanned (EUnary (Spanned UNeg s) e) (span e)),
      prefix "!" (\s e -> Spanned (EUnary (Spanned UNot s) e) (span e))
    ],
    [ binary "*" (\s l r -> Spanned (EBinary (Spanned BMul s) l r) (span l <> span r)),
      binary "/" (\s l r -> Spanned (EBinary (Spanned BDiv s) l r) (span l <> span r)),
      binary "%" (\s l r -> Spanned (EBinary (Spanned BMod s) l r) (span l <> span r))
    ],
    [ binary "+" (\s l r -> Spanned (EBinary (Spanned BAdd s) l r) (span l <> span r)),
      binary "-" (\s l r -> Spanned (EBinary (Spanned BSub s) l r) (span l <> span r))
    ],
    [ binary "=" (\s l r -> Spanned (EBinary (Spanned BEq s) l r) (span l <> span r)),
      binary "!=" (\s l r -> Spanned (EBinary (Spanned BNeq s) l r) (span l <> span r)),
      binary "<" (\s l r -> Spanned (EBinary (Spanned BLt s) l r) (span l <> span r)),
      binary ">" (\s l r -> Spanned (EBinary (Spanned BGt s) l r) (span l <> span r)),
      binary "<=" (\s l r -> Spanned (EBinary (Spanned BLeq s) l r) (span l <> span r)),
      binary ">=" (\s l r -> Spanned (EBinary (Spanned BGeq s) l r) (span l <> span r))
    ],
    [ binary "&&" (\s l r -> Spanned (EBinary (Spanned BAnd s) l r) (span l <> span r)),
      binary "||" (\s l r -> Spanned (EBinary (Spanned BOr s) l r) (span l <> span r))
    ],
    [binary "::" (\s l r -> Spanned (EBinary (Spanned BPair s) l r) (span l <> span r))]
  ]

expr :: Parser (Spanned Expr)
expr = makeExprParser apply operatorTable
  where
    unit :: Parser (Spanned Expr)
    unit = withSpan $ symbol "()" $> EUnit

    litExpr = do
      l <- lit
      pure $ Spanned (ELit l) (span l)

    varExpr = do
      i <- ident
      pure $ Spanned (EVar i) (span i)

    simple :: Parser (Spanned Expr)
    simple = dbg "simple" $ choice [unit, litExpr, varExpr, parens expr]

    lambda :: Parser (Spanned Expr)
    lambda = dbg "lambda" $ withSpan $ ELam <$> (symbol "\\" *> some ident) <*> (symbol "->" *> expr)

    let' :: Parser (Spanned Expr)
    let' =
      dbg "let" $
        withSpan $
          do
            ELet <$> (symbol "let" *> ident)
            <*> (symbol "=" *> expr)
            <*> (symbol "in" *> expr)

    let_rec :: Parser (Spanned Expr)
    let_rec =
      dbg "let_rec" $
        withSpan $
          do
            ELetRec <$> (symbol "let" *> ident)
            <*> some ident
            <*> (symbol "=" *> expr)
            <*> (symbol "in" *> expr)

    if' :: Parser (Spanned Expr)
    if' = dbg "if" $ withSpan $ do
      EIf
        <$> (symbol "if" *> expr)
        <*> (symbol "then" *> expr)
        <*> (symbol "else" *> expr)

    list :: Parser (Spanned Expr)
    list = dbg "list" $ withSpan $ EList <$> brackets (expr `sepEndBy` symbol ",")

    atom :: Parser (Spanned Expr)
    atom =
      dbg "atom" $
        choice
          [ try let_rec <|> let',
            lambda,
            if',
            list,
            simple
          ]

    apply :: Parser (Spanned Expr)
    apply = dbg "apply" $ do
      fargs <- some atom
      pure $ foldl1 (\f a -> Spanned (EApp f a) (span f <> span a)) fargs

decl :: Parser (Spanned Decl)
decl = withSpan $ try fn <|> def
  where
    def = DDef <$> (symbol "def" *> ident) <*> (symbol "=" *> expr)
    fn = DFn <$> (symbol "def" *> ident) <*> some ident <*> (symbol "=" *> expr)

root :: Parser (Spanned Root)
root = withSpan $ Root <$> many decl <* eof

-- parse one decl or expr then wrap in a root
repl :: Parser (Spanned Root)
repl = sc *> (try declParser <|> exprParser)
  where
    declParser = do
      d <- decl <* eof
      pure $ Spanned (Root [d]) (span d)

    exprParser = do
      e <- expr <* eof
      let s = Gen $ span e
          mainDecl = Spanned (DDef (Spanned "main" s) e) s
          r = Root [mainDecl]
      pure $ Spanned r s

parse :: Text -> Either (ParseErrorBundle Text Void) (Spanned Root)
parse = Text.Megaparsec.parse root ""

replParse :: Text -> Either (ParseErrorBundle Text Void) (Spanned Root)
replParse = Text.Megaparsec.parse repl ""
