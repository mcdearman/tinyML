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
import TokenStream
import Prelude hiding (span)
import Token

type Parser = Parsec Void TokenStream

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  start <- lookAhead . try $ token (\(WithPos _ _ s _ _ _) -> Just s) Set.empty
  res <- p
  end <- lookAhead . try $ token (\(WithPos _ _ _ e _ _) -> Just e) Set.empty
  pure $ Spanned res (SrcLoc start end)

tokenWithSpan :: Token -> Parser (Spanned Token)
tokenWithSpan t = token (\(WithPos _ _ s e _ t') -> if t == t' then Just (Spanned t (SrcLoc s e)) else Nothing) Set.empty

int :: Parser (Spanned Int)
int = token (\case (WithPos _ _ s e _ (TInt n)) -> Just (Spanned n (SrcLoc s e)); _ -> Nothing) Set.empty

bool :: Parser (Spanned Bool)
bool = token (\case (WithPos _ _ s e _ (TBool b)) -> Just (Spanned b (SrcLoc s e)); _ -> Nothing) Set.empty

string :: Parser (Spanned Text)
string = token (\case (WithPos _ _ s e _ (TString str)) -> Just (Spanned str (SrcLoc s e)); _ -> Nothing) Set.empty

unit :: Parser (Spanned ())
unit = withSpan $ tokenWithSpan TLParen *> tokenWithSpan TRParen $> ()

lit :: Parser (Spanned Lit)
lit =
  choice
    [ fmap LInt <$> int,
      fmap LBool <$> bool,
      fmap LString <$> string
    ]

ident :: Parser (Spanned Text)
ident = token (\case (WithPos _ _ s e _ (TIdent i)) -> Just (Spanned i (SrcLoc s e)); _ -> Nothing) Set.empty

tyVar :: Parser (Spanned Text)
tyVar = token (\case (WithPos _ _ s e _ (TTyVar v)) -> Just (Spanned v (SrcLoc s e)); _ -> Nothing) Set.empty

typeIdent :: Parser (Spanned Text)
typeIdent = token (\case (WithPos _ _ s e _ (TTypeIdent i)) -> Just (Spanned i (SrcLoc s e)); _ -> Nothing) Set.empty

parens :: Parser a -> Parser a
parens = between (tokenWithSpan TLParen) (tokenWithSpan TRParen)

brackets :: Parser a -> Parser a
brackets = between (tokenWithSpan TLBracket) (tokenWithSpan TRBracket)

arrBrackets :: Parser a -> Parser a
arrBrackets = between (tokenWithSpan THash *> tokenWithSpan TLBracket) (tokenWithSpan TRBracket)

braces :: Parser a -> Parser a
braces = between (tokenWithSpan TLBrace) (tokenWithSpan TRBrace)

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
pattern' = withSpan $ choice [wildcard, litP, varP, pairP, listP, unitP]
  where
    wildcard = tokenWithSpan TUnderscore $> PWildcard
    litP = PLit <$> lit
    varP = PVar <$> ident
    pairP = parens $ PPair <$> pattern' <*> (tokenWithSpan TDoubleColon *> pattern')
    listP = PList <$> brackets (pattern' `sepEndBy` tokenWithSpan TComma)
    unitP = unit $> PUnit

type' :: Parser (Spanned TypeHint)
type' = try (withSpan arrowType) <|> baseType
  where
    arrowType :: Parser TypeHint
    arrowType = THArrow <$> (baseType <* tokenWithSpan TArrow) <*> type'

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
          ((:) <$> (type' <* tokenWithSpan TComma) <*> type' `sepEndBy1` tokenWithSpan TComma)
    recordType =
      THRecord
        <$> optional typeIdent
        <*> braces (((,) <$> typeIdent <*> (tokenWithSpan TColon *> type')) `sepEndBy1` tokenWithSpan TComma)
    unitType = tokenWithSpan TLParen *> tokenWithSpan TRParen $> THUnit

expr :: Parser (Spanned Expr)
expr = makeExprParser apply operatorTable
  where
    unit' = withSpan $ unit $> EUnit
    litExpr = withSpan $ ELit <$> lit
    varExpr = withSpan $ EVar <$> ident

    simple :: Parser (Spanned Expr)
    simple = choice [litExpr, varExpr, try unit' <|> parens expr]

    lambda :: Parser (Spanned Expr)
    lambda = withSpan $ ELam <$> some pattern' <*> (tokenWithSpan TArrow *> expr)

    let' :: Parser (Spanned Expr)
    let' =
      withSpan $
        ELet
          <$> (tokenWithSpan TLet *> pattern')
          <*> (tokenWithSpan TEq *> expr)
          <*> (tokenWithSpan TIn *> expr)

    let_rec :: Parser (Spanned Expr)
    let_rec =
      withSpan $
        ELetRec
          <$> (tokenWithSpan TLet *> ident)
          <*> some pattern'
          <*> (tokenWithSpan TEq *> expr)
          <*> (tokenWithSpan TIn *> expr)

    if' :: Parser (Spanned Expr)
    if' =
      withSpan $
        EIf
          <$> (tokenWithSpan TIf *> expr)
          <*> (tokenWithSpan TThen *> expr)
          <*> (tokenWithSpan TElse *> expr)

    match :: Parser (Spanned Expr)
    match =
      withSpan $
        EMatch
          <$> (tokenWithSpan TMatch *> expr)
          <*> ( tokenWithSpan TWith
                  *> some
                    ( tokenWithSpan TBar
                        *> ((,) <$> pattern' <*> (tokenWithSpan TArrow *> expr))
                    )
              )

    list :: Parser (Spanned Expr)
    list = withSpan $ EList <$> brackets (expr `sepEndBy` tokenWithSpan TComma)

    array :: Parser (Spanned Expr)
    array = withSpan $ do
      a <- arrBrackets (expr `sepEndBy` tokenWithSpan TComma)
      pure $ EArray $ listArray (0, length a - 1) a

    tuple :: Parser (Spanned Expr)
    tuple =
      withSpan $
        ETuple
          <$> parens
            ( (:)
                <$> (expr <* tokenWithSpan TComma)
                <*> expr `sepEndBy1` tokenWithSpan TComma
            )

    record :: Parser (Spanned Expr)
    record = withSpan $ do
      name <- optional typeIdent
      r <- braces (((,) <$> ident <*> (tokenWithSpan TEq *> expr)) `sepEndBy1` tokenWithSpan TComma)
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
    def = DDef <$> (tokenWithSpan TDef *> pattern') <*> (tokenWithSpan TEq *> expr)

    fn :: Parser Decl
    fn = DFn <$> (tokenWithSpan TDef *> ident) <*> some pattern' <*> (tokenWithSpan TEq *> expr)

    fnMatch :: Parser Decl
    fnMatch =
      DFnMatch
        <$> (tokenWithSpan TDef *> ident)
        <*> optional (tokenWithSpan TColon *> type')
        <*> some (tokenWithSpan TBar *> ((,) <$> some pattern' <*> (tokenWithSpan TEq *> expr)))

    record :: Parser Decl
    record =
      DRecordDef
        <$> (tokenWithSpan TData *> typeIdent)
        <*> many tyVar
        <* tokenWithSpan TEq
        <*> braces
          ( ( (,)
                <$> ident
                <*> (tokenWithSpan TColon *> type')
            )
              `sepEndBy1` tokenWithSpan TComma
          )

    dataDef :: Parser Decl
    dataDef =
      DData
        <$> (tokenWithSpan TData *> typeIdent)
        <*> many tyVar
        <*> (tokenWithSpan TEq *> (dataCtor `sepEndBy1` tokenWithSpan TBar))
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
