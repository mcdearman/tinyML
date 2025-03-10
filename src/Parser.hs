module Parser where

import AST
import Common
import Control.Applicative (empty, optional, (<|>))
import Control.Monad.Combinators.Expr
import Data.Array (Array, listArray)
import Data.Functor (($>))
import Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void
import GHC.IO.Handle (Handle)
import Lexer (TokenStream, WithPos (WithPos))
import Text.Megaparsec
  ( MonadParsec (eof, getParserState, lookAhead, notFollowedBy, takeWhile1P, token, try),
    ParseErrorBundle,
    Parsec,
    State (stateInput),
    Stream (take1_),
    between,
    choice,
    getOffset,
    many,
    manyTill,
    parse,
    satisfy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    some,
  )
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Token
import Prelude hiding (span)

type Parser = Parsec Void TokenStream

-- withSpan :: Parser a -> Parser (Spanned a)
-- withSpan p = do
--   startState <- getParserState
--   let input = stateInput startState
--   start <- case take1_ input of
--     Nothing -> pure NoLoc
--     Just (WithPos _ _ s _ _, _) -> pure s
--   res <- p
--   endState <- getParserState
--   let input' = stateInput endState
--   end <- case take1_ input' of
--     Nothing -> pure start
--     Just (WithPos _ _ s _ _, _) -> pure s
--   pure $ Spanned res (start <> end)

tokenWithSpan :: Token -> Parser (Spanned Token)
tokenWithSpan t = token (\(WithPos _ _ s _ t') -> if t == t' then Just (Spanned t s) else Nothing) Set.empty

int :: Parser (Spanned Int)
int = token (\case (WithPos _ _ s _ (TokInt n)) -> Just (Spanned n s); _ -> Nothing) Set.empty

bool :: Parser (Spanned Bool)
bool = token (\case (WithPos _ _ s _ (TokBool b)) -> Just (Spanned b s); _ -> Nothing) Set.empty

string :: Parser (Spanned Text)
string = token (\case (WithPos _ _ s _ (TokString str)) -> Just (Spanned str s); _ -> Nothing) Set.empty

unit :: Parser (Spanned ())
unit = Spanned () <$> (((<>) . span <$> tokenWithSpan TokLParen) <*> (span <$> tokenWithSpan TokRParen))

lit :: Parser (Spanned Lit)
lit =
  choice
    [ fmap LitInt <$> int,
      fmap LitBool <$> bool,
      fmap LitString <$> string
    ]

ident :: Parser (Spanned Text)
ident = token (\case (WithPos _ _ s _ (TokIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

tyVar :: Parser TyVar
tyVar = token (\case (WithPos _ _ s _ (TokTyVar v)) -> Just (Spanned v s); _ -> Nothing) Set.empty

typeIdent :: Parser (Spanned Text)
typeIdent = token (\case (WithPos _ _ s _ (TokTypeIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

parens :: Parser a -> Parser (Spanned a)
parens p = do
  start <- tokenWithSpan TokLParen
  x <- p
  end <- tokenWithSpan TokRParen
  pure $ Spanned x (span start <> span end)

brackets :: Parser a -> Parser (Spanned a)
brackets p = do
  start <- tokenWithSpan TokLBracket
  x <- p
  end <- tokenWithSpan TokRBracket
  pure $ Spanned x (span start <> span end)

arrBrackets :: Parser a -> Parser (Spanned a)
arrBrackets p = do
  start <- tokenWithSpan TokHash
  tokenWithSpan TokLBracket
  x <- p
  end <- tokenWithSpan TokRBracket
  pure $ Spanned x (span start <> span end)

braces :: Parser a -> Parser (Spanned a)
braces p = do
  start <- tokenWithSpan TokLBrace
  x <- p
  end <- tokenWithSpan TokRBrace
  pure $ Spanned x (span start <> span end)

binary :: Token -> (Span -> Spanned Expr -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
binary tok f = InfixL (f . span <$> tokenWithSpan tok)

prefix, postfix :: Token -> (Span -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
prefix tok f = Prefix (f . span <$> tokenWithSpan tok)
postfix tok f = Postfix (f . span <$> tokenWithSpan tok)

operatorTable :: [[Operator Parser (Spanned Expr)]]
operatorTable =
  [ [ prefix TokMinus (\s e -> Spanned (Unary (Spanned UnOpNeg s) e) (span e)),
      prefix TokBang (\s e -> Spanned (Unary (Spanned UnOpNot s) e) (span e))
    ],
    [ binary TokStar (\s l r -> Spanned (Binary (Spanned BinOpMul s) l r) (span l <> span r)),
      binary TokSlash (\s l r -> Spanned (Binary (Spanned BinOpDiv s) l r) (span l <> span r)),
      binary TokPercent (\s l r -> Spanned (Binary (Spanned BinOpMod s) l r) (span l <> span r))
    ],
    [ binary TokPlus (\s l r -> Spanned (Binary (Spanned BinOpAdd s) l r) (span l <> span r)),
      binary TokMinus (\s l r -> Spanned (Binary (Spanned BinOpSub s) l r) (span l <> span r))
    ],
    [ binary TokEq (\s l r -> Spanned (Binary (Spanned BinOpEq s) l r) (span l <> span r)),
      binary TokNeq (\s l r -> Spanned (Binary (Spanned BinOpNeq s) l r) (span l <> span r)),
      binary TokLt (\s l r -> Spanned (Binary (Spanned BinOpLt s) l r) (span l <> span r)),
      binary TokGt (\s l r -> Spanned (Binary (Spanned BinOpGt s) l r) (span l <> span r)),
      binary TokLeq (\s l r -> Spanned (Binary (Spanned BinOpLeq s) l r) (span l <> span r)),
      binary TokGeq (\s l r -> Spanned (Binary (Spanned BinOpGeq s) l r) (span l <> span r))
    ],
    [ binary TokAnd (\s l r -> Spanned (Binary (Spanned BinOpAnd s) l r) (span l <> span r)),
      binary TokOr (\s l r -> Spanned (Binary (Spanned BinOpOr s) l r) (span l <> span r))
    ],
    [binary TokDoubleColon (\s l r -> Spanned (Binary (Spanned BinOpPair s) l r) (span l <> span r))],
    [binary TokPipe (\s l r -> Spanned (Binary (Spanned BinOpPipe s) l r) (span l <> span r))]
  ]

pattern' :: Parser (Spanned Pattern)
pattern' = choice [wildcard, litP, varP, pairP, listP, unitP]
  where
    wildcard = Spanned PatternWildcard . span <$> tokenWithSpan TokUnderscore
    litP = do
      l <- lit
      pure $ Spanned (PatternLit (value l)) (span l)
    varP = do
      i <- ident
      pure $ Spanned (PatternVar i) (span i)
    pairP = parens $ PatternPair <$> pattern' <*> (tokenWithSpan TokDoubleColon *> pattern')
    listP = do
      ps <- brackets $ pattern' `sepEndBy` tokenWithSpan TokComma
      pure $ Spanned (PatternList (value ps)) (span ps)
    unitP = Spanned PatternUnit . span <$> unit

type' :: Parser (Spanned TypeHint)
type' = dbg "type" $ try kindType <|> try arrowType <|> baseType
  where
    arrowType :: Parser (Spanned TypeHint)
    arrowType = do
      t1 <- baseType
      tokenWithSpan TokArrow
      t2 <- type'
      pure $ Spanned (TypeHintArrow t1 t2) (span t1 <> span t2)
    kindType = do
      name <- typeIdent
      ts <- some baseType
      pure $ Spanned (TypeHintKind name ts) (span name <> span (last ts))

    baseType :: Parser (Spanned TypeHint)
    baseType =
      choice
        [ varType,
          identType,
          listType,
          arrayType,
          recordType,
          try (Spanned TypeHintUnit . span <$> unit)
            <|> try tupleType
            <|> parens (value <$> type')
        ]
    varType = do
      v <- tyVar
      pure $ Spanned (TypeHintVar v) (span v)
    identType = do
      i <- typeIdent
      pure $ Spanned (TypeHintIdent i) (span i)
    listType = do
      ts <- brackets type'
      pure $ Spanned (TypeHintList (value ts)) (span ts)
    arrayType = do
      ts <- arrBrackets type'
      pure $ Spanned (TypeHintArray (value ts)) (span ts)
    tupleType =
      fmap TypeHintTuple
        <$> parens
          ( (:)
              <$> (type' <* tokenWithSpan TokComma)
              <*> type' `sepEndBy1` tokenWithSpan TokComma
          )
    recordType = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TokColon *> type')) `sepEndBy1` tokenWithSpan TokComma)
      case name of
        Nothing -> pure $ Spanned (TypeHintRecord Nothing (value r)) (span r)
        Just n -> pure $ Spanned (TypeHintRecord name (value r)) (span n <> span r)

expr :: Parser (Spanned Expr)
expr = makeExprParser apply operatorTable
  where
    unit' = Spanned Unit . span <$> unit
    litExpr = (\l -> Spanned (Lit (value l)) (span l)) <$> lit
    varExpr = (\i -> Spanned (Var i) (span i)) <$> ident

    simple :: Parser (Spanned Expr)
    simple = choice [litExpr, varExpr, try unit' <|> parens (value <$> expr)]

    lambda :: Parser (Spanned Expr)
    lambda = do
      start <- tokenWithSpan TokBackSlash
      ps <- some pattern'
      tokenWithSpan TokArrow
      e <- expr
      pure $ Spanned (Lam ps e) (span start <> span e)

    let' :: Parser (Spanned Expr)
    let' = do
      start <- tokenWithSpan TokLet
      p <- pattern'
      tokenWithSpan TokEq
      e1 <- expr
      tokenWithSpan TokIn
      e2 <- expr
      pure $ Spanned (Let p e1 e2) (span start <> span e2)

    let_rec :: Parser (Spanned Expr)
    let_rec = do
      start <- tokenWithSpan TokLet
      i <- ident
      ps <- some pattern'
      tokenWithSpan TokEq
      e1 <- expr
      tokenWithSpan TokIn
      e2 <- expr
      pure $ Spanned (Fn i ps e1 e2) (span start <> span e2)

    if' :: Parser (Spanned Expr)
    if' = do
      start <- tokenWithSpan TokIf
      cond <- expr
      tokenWithSpan TokThen
      e1 <- expr
      tokenWithSpan TokElse
      e2 <- expr
      pure $ Spanned (If cond e1 e2) (span start <> span e2)

    match :: Parser (Spanned Expr)
    match = do
      start <- tokenWithSpan TokMatch
      e <- expr <* tokenWithSpan TokWith <* tokenWithSpan TokBar
      cases <-
        (((,) <$> pattern' <*> (tokenWithSpan TokArrow *> expr))) `sepBy1` tokenWithSpan TokBar
      pure $ Spanned (Match e cases) (span start <> span (snd (last cases)))

    list :: Parser (Spanned Expr)
    list = fmap List <$> brackets (expr `sepEndBy` tokenWithSpan TokComma)

    array :: Parser (Spanned Expr)
    array = do
      a <- arrBrackets (expr `sepEndBy` tokenWithSpan TokComma)
      pure $ Spanned (Array $ listArray (0, length (value a) - 1) (value a)) (span a)

    tuple :: Parser (Spanned Expr)
    tuple =
      fmap Tuple
        <$> parens
          ( (:)
              <$> (expr <* tokenWithSpan TokComma)
              <*> expr `sepEndBy1` tokenWithSpan TokComma
          )

    record :: Parser (Spanned Expr)
    record = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TokEq *> expr)) `sepEndBy1` tokenWithSpan TokComma)
      case name of
        Nothing -> pure $ Spanned (Record Nothing (value r)) (span r)
        Just n -> pure $ Spanned (Record name (value r)) (span n <> span r)

    atom :: Parser (Spanned Expr)
    atom =
      choice
        [ try let_rec <|> let',
          lambda,
          if',
          match,
          list,
          array,
          try tuple <|> simple,
          record
        ]

    apply :: Parser (Spanned Expr)
    apply = do
      fargs <- some atom
      pure $ foldl1 (\f a -> Spanned (App f a) (span f <> span a)) fargs

decl :: Parser (Spanned Decl)
decl = try fnMatch <|> try fn <|> def <|> try record <|> dataDef
  where
    def :: Parser (Spanned Decl)
    def = do
      start <- tokenWithSpan TokDef
      p <- pattern'
      e <- tokenWithSpan TokAssign *> expr
      pure $ Spanned (DeclDef p e) (span start <> span e)

    fn :: Parser (Spanned Decl)
    fn = do
      start <- tokenWithSpan TokDef
      i <- ident
      ps <- some pattern'
      e <- tokenWithSpan TokAssign *> expr
      pure $ Spanned (DeclFn i ps e) (span start <> span e)

    fnMatch :: Parser (Spanned Decl)
    fnMatch = do
      start <- tokenWithSpan TokDef
      i <- ident
      t <- optional (tokenWithSpan TokColon *> type')
      cases <-
        tokenWithSpan TokBar
          *> ( (,)
                 <$> some pattern'
                 <*> (tokenWithSpan TokAssign *> expr)
             )
            `sepEndBy1` tokenWithSpan TokBar
      pure $ Spanned (DeclFnMatch i t cases) (span start <> span (snd (last cases)))

    record :: Parser (Spanned Decl)
    record =
      do
        start <- tokenWithSpan TokData
        name <- typeIdent
        vars <- many tyVar <* tokenWithSpan TokEq
        p <-
          braces
            ( ( (,)
                  <$> ident
                  <*> (tokenWithSpan TokColon *> type')
              )
                `sepEndBy1` tokenWithSpan TokComma
            )
        pure $ Spanned (DeclRecordDef name vars (value p)) (span start <> span p)

    dataDef :: Parser (Spanned Decl)
    dataDef = do
      name <- tokenWithSpan TokData *> typeIdent
      vars <- many tyVar <* tokenWithSpan TokEq
      p <- ((,) <$> typeIdent <*> many type') `sepEndBy1` tokenWithSpan TokBar
      pure $ Spanned (DeclData name vars p) (span name <> span (last (snd (last p))))

module' :: Text -> Parser Module
module' filename = Module (Spanned filename NoLoc) <$> many decl

repl :: Parser Prog
repl = do
  r <- (try (Right <$> expr) <|> (Left <$> many decl)) <* eof
  pure $ case r of
    Left ds ->
      Spanned
        (Module (Spanned "main" NoLoc) ds)
        (case ds of [] -> NoLoc; d : _ -> span d <> span (last ds))
    Right e ->
      ( Spanned
          ( Module
              (Spanned "main" NoLoc)
              [Spanned (DeclFn (Spanned "main" $ span e) [] e) (span e)]
          )
          (span e)
      )

parseStream :: TokenStream -> Either (ParseErrorBundle TokenStream Void) Prog
parseStream = Text.Megaparsec.parse repl ""
