module Parser where

import AST
import Common
import Control.Applicative (empty, optional, (<|>))
import Control.Monad.Combinators.Expr
import Data.Array (Array, listArray)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.Word (Word8)
import GHC.IO.Handle (Handle)
import Lexer (TokenStream (..), WithPos (..))
import Text.Megaparsec
  ( MonadParsec (eof, getParserState, lookAhead, notFollowedBy, takeWhile1P, token, try),
    ParseErrorBundle,
    Parsec,
    State (stateInput),
    Stream (take1_),
    between,
    choice,
    getInput,
    getOffset,
    many,
    manyTill,
    option,
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

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  -- s <- getInput
  -- start <- case streamTokens <$> s of
  --   [] -> pure NoLoc
  --   t : _ -> pure $ wpSpan t
  start <- fst (streamTokens <$> getInput)
  x <- p
  end <- getInput
  pure $ Spanned x (start <> streamLastSpan end)

tokenWithSpan :: Token -> Parser (Spanned Token)
tokenWithSpan t = token (\(WithPos _ _ s _ t') -> if t == t' then Just (Spanned t s) else Nothing) Set.empty

token' :: Token -> Parser Token
token' t = token (\(WithPos _ _ _ _ t') -> if t == t' then Just t else Nothing) Set.empty

int :: Parser (Spanned Int64)
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
    [ fmap Int <$> int,
      fmap Bool <$> bool,
      fmap String <$> string
    ]

ident :: Parser Name
ident = token (\case (WithPos _ _ s _ (TokIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

tyVar :: Parser TyVar
tyVar = token (\case (WithPos _ _ s _ (TokTyVar v)) -> Just (Spanned v s); _ -> Nothing) Set.empty

typeIdent :: Parser (Spanned Text)
typeIdent = token (\case (WithPos _ _ s _ (TokTypeIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

parens :: Parser a -> Parser a
parens p = token' TokLParen *> p <* token' TokRParen

brackets :: Parser a -> Parser a
brackets p = token' TokLBracket *> p <* token' TokRBracket

arrBrackets :: Parser a -> Parser a
arrBrackets p = token' TokHash *> token' TokLBracket *> p <* token' TokRBracket

braces :: Parser a -> Parser (Spanned a)
braces p = withSpan $ tokenWithSpan TokLBrace *> p <* tokenWithSpan TokRBrace

binary :: Token -> (Span -> Expr -> Expr -> Expr) -> Operator Parser Expr
binary tok f = InfixL (f . span <$> tokenWithSpan tok)

prefix, postfix :: Token -> (Span -> Expr -> Expr) -> Operator Parser Expr
prefix tok f = Prefix (f . span <$> tokenWithSpan tok)
postfix tok f = Postfix (f . span <$> tokenWithSpan tok)

operatorTable :: [[Operator Parser Expr]]
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

pattern' :: Parser Pattern
pattern' = withSpan $ choice [wildcard, litP, varP, pairP, listP, unitP]
  where
    wildcard = tokenWithSpan TokUnderscore $> PatternWildcard
    litP = PatternLit <$> lit
    varP = PatternVar <$> ident
    pairP = parens $ PatternPair <$> pattern' <*> (tokenWithSpan TokDoubleColon *> pattern')
    listP = PatternList <$> brackets $ pattern' `sepEndBy` tokenWithSpan TokComma
    unitP = Spanned PatternUnit . span <$> unit

type' :: Parser TypeAnno
type' = dbg "type" $ try kindType <|> try arrowType <|> baseType
  where
    arrowType :: Parser TypeAnno
    arrowType = do
      t1 <- baseType
      tokenWithSpan TokArrow
      t2 <- type'
      pure $ Spanned (TypeAnnoArrow t1 t2) (span t1 <> span t2)
    kindType = do
      name <- typeIdent
      ts <- some baseType
      pure $ Spanned (TypeAnnoKind name ts) (span name <> span (last ts))

    baseType :: Parser TypeAnno
    baseType =
      choice
        [ varType,
          identType,
          listType,
          arrayType,
          recordType,
          try (Spanned TypeAnnoUnit . span <$> unit)
            <|> try tupleType
            <|> parens (value <$> type')
        ]
    varType = do
      v <- tyVar
      pure $ Spanned (TypeAnnoVar v) (span v)
    identType = do
      i <- typeIdent
      pure $ Spanned (TypeAnnoIdent i) (span i)
    listType = do
      ts <- brackets type'
      pure $ Spanned (TypeAnnoList (value ts)) (span ts)
    arrayType = do
      ts <- arrBrackets type'
      pure $ Spanned (TypeAnnoArray (value ts)) (span ts)
    tupleType =
      fmap TypeAnnoTuple
        <$> parens
          ( (:)
              <$> (type' <* tokenWithSpan TokComma)
              <*> type' `sepEndBy1` tokenWithSpan TokComma
          )
    recordType = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TokColon *> type')) `sepEndBy1` tokenWithSpan TokComma)
      case name of
        Nothing -> pure $ Spanned (TypeAnnoRecord Nothing (value r)) (span r)
        Just n -> pure $ Spanned (TypeAnnoRecord name (value r)) (span n <> span r)

expr :: Parser Expr
expr = makeExprParser apply operatorTable
  where
    unit' = Spanned Unit . span <$> unit
    litExpr = (\l -> Spanned (Lit (value l)) (span l)) <$> lit
    varExpr = (\i -> Spanned (Var i) (span i)) <$> ident

    simple :: Parser Expr
    simple = choice [litExpr, varExpr, try unit' <|> parens (value <$> expr)]

    lambda :: Parser Expr
    lambda = do
      start <- tokenWithSpan TokBackSlash
      ps <- some pattern'
      tokenWithSpan TokArrow
      e <- expr
      pure $ Spanned (Lam ps e) (span start <> span e)

    let' :: Parser Expr
    let' = do
      start <- tokenWithSpan TokLet
      p <- pattern'
      tokenWithSpan TokEq
      e1 <- expr
      tokenWithSpan TokIn
      e2 <- expr
      pure $ Spanned (Let p e1 e2) (span start <> span e2)

    let_rec :: Parser Expr
    let_rec = do
      start <- tokenWithSpan TokLet
      i <- ident
      ps <- some pattern'
      tokenWithSpan TokEq
      e1 <- expr
      tokenWithSpan TokIn
      e2 <- expr
      pure $ Spanned (Fn i ps e1 e2) (span start <> span e2)

    if' :: Parser Expr
    if' = do
      start <- tokenWithSpan TokIf
      cond <- expr
      tokenWithSpan TokThen
      then' <- expr
      tokenWithSpan TokElse
      else' <- expr
      pure $ Spanned (If cond then' else') (span start <> span else')

    match :: Parser Expr
    match = do
      start <- tokenWithSpan TokMatch
      e <- expr <* tokenWithSpan TokWith <* tokenWithSpan TokBar
      cases <-
        ( ( (,)
              <$> pattern'
              <*> (tokenWithSpan TokArrow *> expr)
          )
        )
          `sepBy1` tokenWithSpan TokBar
      pure $ Spanned (Match e cases) (span start <> span (snd (last cases)))

    list :: Parser Expr
    list = fmap List <$> brackets (expr `sepEndBy` tokenWithSpan TokComma)

    array :: Parser Expr
    array = do
      a <- arrBrackets (expr `sepEndBy` tokenWithSpan TokComma)
      pure $ Spanned (Array $ listArray (0, fromIntegral length (value a) - 1) (value a)) (span a)

    tuple :: Parser Expr
    tuple =
      fmap Tuple
        <$> parens
          ( (:)
              <$> (expr <* tokenWithSpan TokComma)
              <*> expr `sepEndBy1` tokenWithSpan TokComma
          )

    record :: Parser Expr
    record = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TokEq *> expr)) `sepEndBy1` tokenWithSpan TokComma)
      case name of
        Nothing -> pure $ Spanned (Record Nothing (value r)) (span r)
        Just n -> pure $ Spanned (Record name (value r)) (span n <> span r)

    atom :: Parser Expr
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

    apply :: Parser Expr
    apply = do
      fargs <- some atom
      pure $ foldl1 (\f a -> Spanned (App f a) (span f <> span a)) fargs

-- decl :: Parser Decl
-- decl = try fnMatch <|> try fn <|> def <|> try record <|> dataDef
--   where
--     def :: Parser Decl
--     def = do
--       start <- tokenWithSpan TokDef
--       p <- pattern'
--       e <- tokenWithSpan TokAssign *> expr
--       pure $ Spanned (DeclDef p e) (span start <> span e)

--     fn :: Parser Decl
--     fn = do
--       start <- tokenWithSpan TokDef
--       i <- ident
--       ps <- some pattern'
--       e <- tokenWithSpan TokAssign *> expr
--       pure $ Spanned (DeclFn i ps e) (span start <> span e)

--     fnMatch :: Parser Decl
--     fnMatch = do
--       start <- tokenWithSpan TokDef
--       i <- ident
--       t <- optional (tokenWithSpan TokColon *> type')
--       cases <-
--         tokenWithSpan TokBar
--           *> ( (,)
--                  <$> some pattern'
--                  <*> (tokenWithSpan TokAssign *> expr)
--              )
--             `sepEndBy1` tokenWithSpan TokBar
--       pure $ Spanned (DeclFnMatch i t cases) (span start <> span (snd (last cases)))

--     record :: Parser Decl
--     record =
--       do
--         start <- tokenWithSpan TokData
--         name <- typeIdent
--         vars <- many tyVar <* tokenWithSpan TokEq
--         p <-
--           braces
--             ( ( (,)
--                   <$> ident
--                   <*> (tokenWithSpan TokColon *> type')
--               )
--                 `sepEndBy1` tokenWithSpan TokComma
--             )
--         pure $ Spanned (DeclRecordDef name vars (value p)) (span start <> span p)

--     dataDef :: Parser Decl
--     dataDef = do
--       name <- tokenWithSpan TokData *> typeIdent
--       vars <- many tyVar <* tokenWithSpan TokEq
--       p <- ((,) <$> typeIdent <*> many type') `sepEndBy1` tokenWithSpan TokBar
--       pure $ Spanned (DeclData name vars p) (span name <> span (last (snd (last p))))

visibility :: Parser Visibility
visibility = option Private (tokenWithSpan TokPub $> Public)

dataDef :: Parser DataDef
dataDef = do
  v <- visibility
  d <- tokenWithSpan TokData
  name <- typeIdent
  vars <- many tyVar <* tokenWithSpan TokEq
  p <- ((,) <$> ident <*> many type') `sepEndBy1` tokenWithSpan TokBar
  pure $ Spanned (DataDef name vars p v) (span d <> span (last (snd (last p))))

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
