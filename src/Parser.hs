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
  ( MonadParsec (eof, lookAhead, notFollowedBy, takeWhile1P, token, try),
    ParseErrorBundle,
    Parsec,
    Stream (take1_),
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
import qualified Token as T
import TokenStream
import Prelude hiding (span)

type Parser = Parsec Void TokenStream

-- withSpan :: Parser a -> Parser (Spanned a)
-- withSpan p = do
--   start <- lookAhead . try $ token (\(WithPos _ _ s _ _ _) -> Just s) Set.empty
--   res <- p
--   end <- lookAhead . try $ token (\(WithPos _ _ _ e _ _) -> Just e) Set.empty
--   pure $ Spanned res (SrcLoc start end)

tokenWithSpan :: T.Token -> Parser (Spanned T.Token)
tokenWithSpan t = token (\(WithPos _ _ s e _ t') -> if t == t' then Just (Spanned t (SrcLoc s e)) else Nothing) Set.empty

int :: Parser (Spanned Int)
int = token (\case (WithPos _ _ s e _ (T.TInt n)) -> Just (Spanned n (SrcLoc s e)); _ -> Nothing) Set.empty

bool :: Parser (Spanned Bool)
bool = token (\case (WithPos _ _ s e _ (T.TBool b)) -> Just (Spanned b (SrcLoc s e)); _ -> Nothing) Set.empty

string :: Parser (Spanned Text)
string = token (\case (WithPos _ _ s e _ (T.TString str)) -> Just (Spanned str (SrcLoc s e)); _ -> Nothing) Set.empty

unit :: Parser (Spanned ())
unit = do
  start <- tokenWithSpan T.TLParen
  end <- tokenWithSpan T.TRParen
  pure $ Spanned () (span start <> span end)

lit :: Parser (Spanned Lit)
lit =
  choice
    [ fmap LInt <$> int,
      fmap LBool <$> bool,
      fmap LString <$> string
    ]

ident :: Parser (Spanned Text)
ident = token (\case (WithPos _ _ s e _ (T.TIdent i)) -> Just (Spanned i (SrcLoc s e)); _ -> Nothing) Set.empty

tyVar :: Parser (Spanned Text)
tyVar = token (\case (WithPos _ _ s e _ (T.TTyVar v)) -> Just (Spanned v (SrcLoc s e)); _ -> Nothing) Set.empty

typeIdent :: Parser (Spanned Text)
typeIdent = token (\case (WithPos _ _ s e _ (T.TTypeIdent i)) -> Just (Spanned i (SrcLoc s e)); _ -> Nothing) Set.empty

parens :: Parser a -> Parser a
parens = between (tokenWithSpan T.TLParen) (tokenWithSpan T.TRParen)

brackets :: Parser a -> Parser a
brackets = between (tokenWithSpan T.TLBracket) (tokenWithSpan T.TRBracket)

arrBrackets :: Parser a -> Parser a
arrBrackets = between (tokenWithSpan T.THash *> tokenWithSpan T.TLBracket) (tokenWithSpan T.TRBracket)

braces :: Parser a -> Parser a
braces = between (tokenWithSpan T.TLBrace) (tokenWithSpan T.TRBrace)

binary :: T.Token -> (Span -> Spanned Expr -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
binary tok f = InfixL (f . span <$> tokenWithSpan tok)

prefix, postfix :: T.Token -> (Span -> Spanned Expr -> Spanned Expr) -> Operator Parser (Spanned Expr)
prefix tok f = Prefix (f . span <$> tokenWithSpan tok)
postfix tok f = Postfix (f . span <$> tokenWithSpan tok)

operatorTable :: [[Operator Parser (Spanned Expr)]]
operatorTable =
  [ [ prefix T.TMinus (\s e -> Spanned (EUnary (Spanned UNeg s) e) (span e)),
      prefix T.TBang (\s e -> Spanned (EUnary (Spanned UNot s) e) (span e))
    ],
    [ binary T.TStar (\s l r -> Spanned (EBinary (Spanned BMul s) l r) (span l <> span r)),
      binary T.TSlash (\s l r -> Spanned (EBinary (Spanned BDiv s) l r) (span l <> span r)),
      binary T.TPercent (\s l r -> Spanned (EBinary (Spanned BMod s) l r) (span l <> span r))
    ],
    [ binary T.TPlus (\s l r -> Spanned (EBinary (Spanned BAdd s) l r) (span l <> span r)),
      binary T.TMinus (\s l r -> Spanned (EBinary (Spanned BSub s) l r) (span l <> span r))
    ],
    [ binary T.TEq (\s l r -> Spanned (EBinary (Spanned BEq s) l r) (span l <> span r)),
      binary T.TNeq (\s l r -> Spanned (EBinary (Spanned BNeq s) l r) (span l <> span r)),
      binary T.TLt (\s l r -> Spanned (EBinary (Spanned BLt s) l r) (span l <> span r)),
      binary T.TGt (\s l r -> Spanned (EBinary (Spanned BGt s) l r) (span l <> span r)),
      binary T.TLeq (\s l r -> Spanned (EBinary (Spanned BLeq s) l r) (span l <> span r)),
      binary T.TGeq (\s l r -> Spanned (EBinary (Spanned BGeq s) l r) (span l <> span r))
    ],
    [ binary T.TAnd (\s l r -> Spanned (EBinary (Spanned BAnd s) l r) (span l <> span r)),
      binary T.TOr (\s l r -> Spanned (EBinary (Spanned BOr s) l r) (span l <> span r))
    ],
    [binary T.TDoubleColon (\s l r -> Spanned (EBinary (Spanned BPair s) l r) (span l <> span r))]
  ]

pattern' :: Parser (Spanned Pattern)
pattern' = choice [wildcard, litP, varP, pairP, listP, unitP]
  where
    wildcard = Spanned PWildcard . span <$> tokenWithSpan T.TUnderscore
    litP = PLit <$> lit
    varP = PVar <$> ident
    pairP = parens $ PPair <$> pattern' <*> (tokenWithSpan T.TDoubleColon *> pattern')
    listP = PList <$> brackets (pattern' `sepEndBy` tokenWithSpan T.TComma)
    unitP = unit $> PUnit

type' :: Parser (Spanned TypeHint)
type' = try (withSpan arrowType) <|> baseType
  where
    arrowType :: Parser TypeHint
    arrowType = THArrow <$> (baseType <* tokenWithSpan T.TArrow) <*> type'

    baseType :: Parser (Spanned TypeHint)
    baseType =
      withSpan $
        choice
          [ varType,
            try kindType <|> THIdent <$> typeIdent,
            listType,
            arrayType,
            unitType,
            recordType,
            try tupleType <|> parens (value <$> type')
          ]
    varType = THVar <$> tyVar
    kindType = THKind <$> typeIdent <*> some type'
    listType = THList <$> brackets type'
    arrayType = THArray <$> arrBrackets type'
    tupleType =
      THTuple
        <$> parens
          ((:) <$> (type' <* tokenWithSpan T.TComma) <*> type' `sepEndBy1` tokenWithSpan T.TComma)
    recordType =
      THRecord
        <$> optional typeIdent
        <*> braces (((,) <$> typeIdent <*> (tokenWithSpan T.TColon *> type')) `sepEndBy1` tokenWithSpan T.TComma)
    unitType = tokenWithSpan T.TLParen *> tokenWithSpan T.TRParen $> THUnit

expr :: Parser (Spanned Expr)
expr = makeExprParser apply operatorTable
  where
    unit' = withSpan $ unit $> EUnit
    litExpr = withSpan $ ELit <$> lit
    varExpr = withSpan $ EVar <$> ident

    simple :: Parser (Spanned Expr)
    simple = choice [litExpr, varExpr, try unit' <|> parens expr]

    lambda :: Parser (Spanned Expr)
    lambda = do
      start <- tokenWithSpan T.TBackSlash
      args <- some pattern'
      _ <- tokenWithSpan T.TArrow
      body <- expr
      pure $ Spanned (ELam args body) (span start <> span body)

    let' :: Parser (Spanned Expr)
    let' =
      withSpan $
        do
          ELet <$> (tokenWithSpan T.TLet *> pattern')
          <*> (tokenWithSpan T.TEq *> expr)
          <*> (tokenWithSpan T.TIn *> expr)

    let_rec :: Parser (Spanned Expr)
    let_rec =
      withSpan $
        do
          ELetRec <$> (tokenWithSpan T.TLet *> ident)
          <*> some pattern'
          <*> (tokenWithSpan T.TEq *> expr)
          <*> (tokenWithSpan T.TIn *> expr)

    if' :: Parser (Spanned Expr)
    if' = withSpan $ do
      EIf
        <$> (tokenWithSpan T.TIf *> expr)
        <*> (tokenWithSpan T.TThen *> expr)
        <*> (tokenWithSpan T.TElse *> expr)

    match :: Parser (Spanned Expr)
    match = withSpan $ do
      EMatch
        <$> (tokenWithSpan T.TMatch *> expr)
        <*> ( tokenWithSpan T.TWith
                *> some
                  ( tokenWithSpan T.TBar
                      *> ((,) <$> pattern' <*> (tokenWithSpan T.TArrow *> expr))
                  )
            )

    list :: Parser (Spanned Expr)
    list = withSpan $ EList <$> brackets (expr `sepEndBy` tokenWithSpan T.TComma)

    array :: Parser (Spanned Expr)
    array = withSpan $ do
      a <- arrBrackets (expr `sepEndBy` tokenWithSpan T.TComma)
      pure $ EArray $ listArray (0, length a - 1) a

    tuple :: Parser (Spanned Expr)
    tuple =
      withSpan $
        ETuple
          <$> parens
            ( (:)
                <$> (expr <* tokenWithSpan T.TComma)
                <*> expr `sepEndBy1` tokenWithSpan T.TComma
            )

    record :: Parser (Spanned Expr)
    record = withSpan $ do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan T.TEq *> expr)) `sepEndBy1` tokenWithSpan T.TComma)
      pure $ ERecord name r

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
decl = withSpan $ try fnMatch <|> try fn <|> def <|> try record <|> dataDef
  where
    def :: Parser Decl
    def = DDef <$> (tokenWithSpan T.TDef *> pattern') <*> (tokenWithSpan T.TEq *> expr)

    fn :: Parser Decl
    fn = DFn <$> (tokenWithSpan T.TDef *> ident) <*> some pattern' <*> (tokenWithSpan T.TEq *> expr)

    fnMatch :: Parser Decl
    fnMatch =
      DFnMatch
        <$> (tokenWithSpan T.TDef *> ident)
        <*> optional (tokenWithSpan T.TColon *> type')
        <*> some (tokenWithSpan T.TBar *> ((,) <$> some pattern' <*> (tokenWithSpan T.TEq *> expr)))

    record :: Parser Decl
    record =
      DRecordDef
        <$> (tokenWithSpan T.TData *> typeIdent)
        <*> many tyVar
        <* tokenWithSpan T.TEq
        <*> braces
          ( ( (,)
                <$> ident
                <*> (tokenWithSpan T.TColon *> type')
            )
              `sepEndBy1` tokenWithSpan T.TComma
          )

    dataDef :: Parser Decl
    dataDef =
      DData
        <$> (tokenWithSpan T.TData *> typeIdent)
        <*> many tyVar
        <*> (tokenWithSpan T.TEq *> (dataCtor `sepEndBy1` tokenWithSpan T.TBar))
      where
        dataCtor = (,) <$> typeIdent <*> many type'

module' :: Text -> Parser Module
module' filename = Module (Spanned filename NoLoc) <$> many decl

repl :: Parser (Spanned Program)
repl = withSpan $ PRepl <$> (try declParser <|> exprParser) <* eof
  where
    declParser = do
      d <- decl
      pure $ Spanned (Module (Spanned "main" NoLoc) [d]) (span d)

    exprParser = do
      e <- expr
      let s = Gen $ span e
          mainDecl = Spanned (DFn (Spanned "main" s) [] e) s
          m = Module (Spanned "main" NoLoc) [mainDecl]
      pure $ Spanned m s

parseStream :: TokenStream -> Either (ParseErrorBundle TokenStream Void) (Spanned Program)
parseStream = Text.Megaparsec.parse repl ""
