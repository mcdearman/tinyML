module Parser (parseStream) where

import AST
import Control.Applicative (empty, optional, (<|>))
import Control.Monad.Combinators.Expr
import Data.Array (listArray)
import Data.Functor (($>))
import Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Text.IO (hGetContents)
import Data.Void
import GHC.IO.Handle (Handle)
import Span
import Spanned
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
    sepEndBy,
    sepEndBy1,
    some,
  )
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Token
import TokenStream hiding (span)
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
int = token (\case (WithPos _ _ s _ (TInt n)) -> Just (Spanned n s); _ -> Nothing) Set.empty

bool :: Parser (Spanned Bool)
bool = token (\case (WithPos _ _ s _ (TBool b)) -> Just (Spanned b s); _ -> Nothing) Set.empty

string :: Parser (Spanned Text)
string = token (\case (WithPos _ _ s _ (TString str)) -> Just (Spanned str s); _ -> Nothing) Set.empty

unit :: Parser (Spanned ())
unit = Spanned () <$> (((<>) . span <$> tokenWithSpan TLParen) <*> (span <$> tokenWithSpan TRParen))

lit :: Parser (Spanned Lit)
lit =
  choice
    [ fmap LInt <$> int,
      fmap LBool <$> bool,
      fmap LString <$> string
    ]

ident :: Parser (Spanned Text)
ident = token (\case (WithPos _ _ s _ (TIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

tyVar :: Parser TyVar
tyVar = token (\case (WithPos _ _ s _ (TTyVar v)) -> Just (Spanned v s); _ -> Nothing) Set.empty

typeIdent :: Parser (Spanned Text)
typeIdent = token (\case (WithPos _ _ s _ (TTypeIdent i)) -> Just (Spanned i s); _ -> Nothing) Set.empty

parens :: Parser a -> Parser (Spanned a)
parens p = do
  start <- tokenWithSpan TLParen
  x <- p
  end <- tokenWithSpan TRParen
  pure $ Spanned x (span start <> span end)

brackets :: Parser a -> Parser (Spanned a)
brackets p = do
  start <- tokenWithSpan TLBracket
  x <- p
  end <- tokenWithSpan TRBracket
  pure $ Spanned x (span start <> span end)

arrBrackets :: Parser a -> Parser (Spanned a)
arrBrackets p = do
  start <- tokenWithSpan THash
  tokenWithSpan TLBracket
  x <- p
  end <- tokenWithSpan TRBracket
  pure $ Spanned x (span start <> span end)

braces :: Parser a -> Parser (Spanned a)
braces p = do
  start <- tokenWithSpan TLBrace
  x <- p
  end <- tokenWithSpan TRBrace
  pure $ Spanned x (span start <> span end)

binary :: Token -> (Span -> Spanned Expr -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
binary tok f = InfixL (f . span <$> tokenWithSpan tok)

prefix, postfix :: Token -> (Span -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
prefix tok f = Prefix (f . span <$> tokenWithSpan tok)
postfix tok f = Postfix (f . span <$> tokenWithSpan tok)

operatorTable :: [[Operator Parser (Spanned Expr)]]
operatorTable =
  [ [ prefix TMinus (\s e -> Spanned (EUnary (Spanned UNeg s) e) (span e)),
      prefix TBang (\s e -> Spanned (EUnary (Spanned UNot s) e) (span e))
    ],
    [ binary TStar (\s l r -> Spanned (EBinary (Spanned BMul s) l r) (span l <> span r)),
      binary TSlash (\s l r -> Spanned (EBinary (Spanned BDiv s) l r) (span l <> span r)),
      binary TPercent (\s l r -> Spanned (EBinary (Spanned BMod s) l r) (span l <> span r))
    ],
    [ binary TPlus (\s l r -> Spanned (EBinary (Spanned BAdd s) l r) (span l <> span r)),
      binary TMinus (\s l r -> Spanned (EBinary (Spanned BSub s) l r) (span l <> span r))
    ],
    [ binary TEq (\s l r -> Spanned (EBinary (Spanned BEq s) l r) (span l <> span r)),
      binary TNeq (\s l r -> Spanned (EBinary (Spanned BNeq s) l r) (span l <> span r)),
      binary TLt (\s l r -> Spanned (EBinary (Spanned BLt s) l r) (span l <> span r)),
      binary TGt (\s l r -> Spanned (EBinary (Spanned BGt s) l r) (span l <> span r)),
      binary TLeq (\s l r -> Spanned (EBinary (Spanned BLeq s) l r) (span l <> span r)),
      binary TGeq (\s l r -> Spanned (EBinary (Spanned BGeq s) l r) (span l <> span r))
    ],
    [ binary TAnd (\s l r -> Spanned (EBinary (Spanned BAnd s) l r) (span l <> span r)),
      binary TOr (\s l r -> Spanned (EBinary (Spanned BOr s) l r) (span l <> span r))
    ],
    [binary TDoubleColon (\s l r -> Spanned (EBinary (Spanned BPair s) l r) (span l <> span r))]
  ]

pattern' :: Parser (Spanned Pattern)
pattern' = choice [wildcard, litP, varP, pairP, listP, unitP]
  where
    wildcard = Spanned PWildcard . span <$> tokenWithSpan TUnderscore
    litP = do
      l <- lit
      pure $ Spanned (PLit l) (span l)
    varP = do
      i <- ident
      pure $ Spanned (PVar i) (span i)
    pairP = parens $ PPair <$> pattern' <*> (tokenWithSpan TDoubleColon *> pattern')
    listP = do
      ps <- brackets $ pattern' `sepEndBy` tokenWithSpan TComma
      pure $ Spanned (PList (value ps)) (span ps)
    unitP = Spanned PUnit . span <$> unit

type' :: Parser (Spanned TypeHint)
type' = dbg "type" $ try kindType <|> try arrowType <|> baseType
  where
    arrowType :: Parser (Spanned TypeHint)
    arrowType = do
      t1 <- baseType
      tokenWithSpan TArrow
      t2 <- type'
      pure $ Spanned (THArrow t1 t2) (span t1 <> span t2)
    kindType = do
      name <- typeIdent
      ts <- some baseType
      pure $ Spanned (THKind name ts) (span name <> span (last ts))

    baseType :: Parser (Spanned TypeHint)
    baseType =
      choice
        [ varType,
          identType,
          listType,
          arrayType,
          recordType,
          try (Spanned THUnit . span <$> unit)
            <|> try tupleType
            <|> parens (value <$> type')
        ]
    varType = do
      v <- tyVar
      pure $ Spanned (THVar v) (span v)
    identType = do
      i <- typeIdent
      pure $ Spanned (THIdent i) (span i)
    listType = do
      ts <- brackets type'
      pure $ Spanned (THList (value ts)) (span ts)
    arrayType = do
      ts <- arrBrackets type'
      pure $ Spanned (THArray (value ts)) (span ts)
    tupleType =
      fmap THTuple
        <$> parens
          ( (:)
              <$> (type' <* tokenWithSpan TComma)
              <*> type' `sepEndBy1` tokenWithSpan TComma
          )
    recordType = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TColon *> type')) `sepEndBy1` tokenWithSpan TComma)
      case name of
        Nothing -> pure $ Spanned (THRecord Nothing (value r)) (span r)
        Just n -> pure $ Spanned (THRecord name (value r)) (span n <> span r)

expr :: Parser (Spanned Expr)
expr = makeExprParser apply operatorTable
  where
    unit' = Spanned EUnit . span <$> unit
    litExpr = (\l -> Spanned (ELit l) (span l)) <$> lit
    varExpr = (\i -> Spanned (EVar i) (span i)) <$> ident

    simple :: Parser (Spanned Expr)
    simple = choice [litExpr, varExpr, try unit' <|> parens (value <$> expr)]

    lambda :: Parser (Spanned Expr)
    lambda = do
      start <- tokenWithSpan TBackSlash
      ps <- some pattern'
      tokenWithSpan TArrow
      e <- expr
      pure $ Spanned (ELam ps e) (span start <> span e)

    let' :: Parser (Spanned Expr)
    let' = do
      start <- tokenWithSpan TLet
      p <- pattern'
      tokenWithSpan TEq
      e1 <- expr
      tokenWithSpan TIn
      e2 <- expr
      pure $ Spanned (ELet p e1 e2) (span start <> span e2)

    let_rec :: Parser (Spanned Expr)
    let_rec = do
      start <- tokenWithSpan TLet
      i <- ident
      ps <- some pattern'
      tokenWithSpan TEq
      e1 <- expr
      tokenWithSpan TIn
      e2 <- expr
      pure $ Spanned (EFn i ps e1 e2) (span start <> span e2)

    if' :: Parser (Spanned Expr)
    if' = do
      start <- tokenWithSpan TIf
      cond <- expr
      tokenWithSpan TThen
      e1 <- expr
      tokenWithSpan TElse
      e2 <- expr
      pure $ Spanned (EIf cond e1 e2) (span start <> span e2)

    match :: Parser (Spanned Expr)
    match = do
      start <- tokenWithSpan TMatch
      e <- expr
      tokenWithSpan TWith
      cases <-
        ( tokenWithSpan TBar
            *> ( (,)
                   <$> pattern'
                   <*> (tokenWithSpan TArrow *> expr)
               )
          )
          `sepEndBy1` tokenWithSpan TBar
      pure $ Spanned (EMatch e cases) (span start <> span (snd (last cases)))

    list :: Parser (Spanned Expr)
    list = fmap EList <$> brackets (expr `sepEndBy` tokenWithSpan TComma)

    array :: Parser (Spanned Expr)
    array = do
      a <- arrBrackets (expr `sepEndBy` tokenWithSpan TComma)
      pure $ Spanned (EArray $ listArray (0, length (value a) - 1) (value a)) (span a)

    tuple :: Parser (Spanned Expr)
    tuple =
      fmap ETuple
        <$> parens
          ( (:)
              <$> (expr <* tokenWithSpan TComma)
              <*> expr `sepEndBy1` tokenWithSpan TComma
          )

    record :: Parser (Spanned Expr)
    record = do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TEq *> expr)) `sepEndBy1` tokenWithSpan TComma)
      case name of
        Nothing -> pure $ Spanned (ERecord Nothing (value r)) (span r)
        Just n -> pure $ Spanned (ERecord name (value r)) (span n <> span r)

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
      pure $ foldl1 (\f a -> Spanned (EApp f a) (span f <> span a)) fargs

decl :: Parser (Spanned Decl)
decl = try fnMatch <|> try fn <|> def <|> try record <|> dataDef
  where
    def :: Parser (Spanned Decl)
    def = do
      start <- tokenWithSpan TDef
      p <- pattern'
      e <- tokenWithSpan TEq *> expr
      pure $ Spanned (DDef p e) (span start <> span e)

    fn :: Parser (Spanned Decl)
    fn = do
      start <- tokenWithSpan TDef
      i <- ident
      ps <- some pattern'
      e <- tokenWithSpan TEq *> expr
      pure $ Spanned (DFn i ps e) (span start <> span e)

    fnMatch :: Parser (Spanned Decl)
    fnMatch = do
      start <- tokenWithSpan TDef
      i <- ident
      t <- optional (tokenWithSpan TColon *> type')
      cases <-
        tokenWithSpan TBar
          *> ( (,)
                 <$> some pattern'
                 <*> (tokenWithSpan TEq *> expr)
             )
            `sepEndBy1` tokenWithSpan TBar
      pure $ Spanned (DFnMatch i t cases) (span start <> span (snd (last cases)))

    record :: Parser (Spanned Decl)
    record =
      do
        start <- tokenWithSpan TData
        name <- typeIdent
        vars <- many tyVar <* tokenWithSpan TEq
        p <-
          braces
            ( ( (,)
                  <$> ident
                  <*> (tokenWithSpan TColon *> type')
              )
                `sepEndBy1` tokenWithSpan TComma
            )
        pure $ Spanned (DRecordDef name vars (value p)) (span start <> span p)

    dataDef :: Parser (Spanned Decl)
    dataDef = do
      name <- tokenWithSpan TData *> typeIdent
      vars <- many tyVar <* tokenWithSpan TEq
      p <- ((,) <$> typeIdent <*> many type') `sepEndBy1` tokenWithSpan TBar
      pure $ Spanned (DData name vars p) (span name <> span (last (snd (last p))))

module' :: Text -> Parser Module
module' filename = Module (Spanned filename NoLoc) <$> many decl

repl :: Parser (Spanned Program)
repl = do
  r <- (try (Left <$> decl) <|> Right <$> expr) <* eof
  pure (Spanned (PRepl r) NoLoc)

parseStream :: TokenStream -> Either (ParseErrorBundle TokenStream Void) (Spanned Program)
parseStream = Text.Megaparsec.parse repl ""
