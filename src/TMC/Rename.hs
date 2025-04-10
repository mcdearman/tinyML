module TMC.Rename where

import qualified AST as A
import Common
import Control.Monad.State.Strict
import Control.Placeholder (todo)
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import NIR
import qualified Parser as A
import Prelude hiding (span)

newtype RenameError = UnboundVariable Span deriving (Show, Eq)

newtype Env = Env [Frame] deriving (Show, Eq)

instance Semigroup Env where
  Env fs <> Env fs' = Env (fs <> fs')

type Frame = Map Text ResId

data Resolver = Resolver
  { resId :: ResId,
    env :: Env,
    errors :: [RenameError]
  }
  deriving (Show, Eq)

defaultResolver :: Resolver
defaultResolver = execState defaultEnv (Resolver (Id 0) (Env []) [])

type ResState a = State Resolver a

builtins :: [Text]
builtins =
  [ "neg",
    "not",
    "add",
    "sub",
    "mul",
    "/",
    "%",
    "^",
    "=",
    "!=",
    ">",
    "<",
    ">=",
    "<=",
    "::",
    "|>"
  ]

defaultEnv :: ResState Env
defaultEnv = do
  let f = Map.fromList [(n, Id i) | (i, n) <- zip [0 ..] builtins]
  modify' $ \r -> r {resId = Id $ length builtins, env = Env [f]}
  pure $ Env [f]

push :: ResState ()
push = do
  r@Resolver {env = Env fs} <- get
  put r {env = Env $ Map.empty : fs}

pop :: ResState ()
pop = do
  r@Resolver {env = Env fs} <- get
  case fs of
    [] -> error "cannot pop empty environment"
    _ : [] -> error "cannot pop top-level environment"
    _ : fs' -> do
      put r {env = Env fs'}

define :: Text -> ResState ResId
define n = do
  r@Resolver {resId = i@(Id i'), env = Env fs, errors = _} <- get
  case fs of
    [] -> do
      let f = Map.singleton n i
      put r {resId = Id $ i' + 1, env = Env [f]}
    f : fs' -> do
      let f' = Map.insert n i f
      put r {resId = Id $ i' + 1, env = Env $ f' : fs'}
  pure i

lookupOrDefine :: Text -> ResState ResId
lookupOrDefine n = do
  Resolver {env = e} <- get
  case lookup' n e of
    Just i -> pure i
    Nothing -> define n

lookupVar :: A.Name -> ResState ResId
lookupVar n = do
  Resolver {env = e} <- get
  case lookup' (value n) e of
    Just i -> pure i
    Nothing -> do
      pushError $ UnboundVariable (span n)
      pure $ Id 0

lookup' :: Text -> Env -> Maybe ResId
lookup' _ (Env []) = Nothing
lookup' n' (Env (f : fs)) = case Map.lookup n' f of
  Just i -> Just i
  Nothing -> lookup' n' (Env fs)

pushError :: RenameError -> ResState ()
pushError e = do
  r@Resolver {errors = es} <- get
  put r {errors = e : es}

rename :: A.Prog -> ResState Prog
rename (Spanned (A.Module (Spanned n s) ds) s') = do
  n' <- lookupOrDefine n
  ds' <- traverse renameDecl ds
  pure $ Spanned (Module (Spanned (n, n') s) ds') s'

renameDecl :: Spanned A.Decl -> ResState (Spanned Decl)
renameDecl (Spanned (A.DeclDef p e) s) = do
  p' <- renamePattern p
  e' <- renameExpr e
  pure $ Spanned (DDef p' e') s
renameDecl (Spanned (A.DeclFn (Spanned n s) ps e) s') = do
  n' <- define n
  push
  ps' <- traverse renamePattern ps
  e' <- renameExpr e
  pop
  pure $ Spanned (DFn (Spanned (n, n') s) ps' e') s'
renameDecl (Spanned (A.DeclFnMatch (Spanned n s) t cs) s') = do
  n' <- define n
  t' <- traverse renameTypeHint t
  push
  cs' <- traverse (\(ps, e) -> (,) <$> traverse renamePattern ps <*> renameExpr e) cs
  pop
  pure $ Spanned (DFnMatch (Spanned (n, n') s) t' cs') s'
renameDecl _ = undefined

renameExpr :: Spanned A.Expr -> ResState (Spanned Expr)
renameExpr (Spanned (A.Lit lit) s) = pure $ Spanned (ELit (renameLit lit)) s
renameExpr (Spanned (A.Var v) s) = do
  v' <- lookupVar v
  pure $ Spanned (EVar (Spanned (value v, v') s)) s
renameExpr (Spanned (A.App f arg) s) = do
  f' <- renameExpr f
  arg' <- renameExpr arg
  pure $ Spanned (EApp f' arg') s
renameExpr (Spanned (A.Lam ps e) s) = do
  push
  ps' <- traverse renamePattern ps
  e' <- renameExpr e
  pop
  pure $ foldr (\p expr -> Spanned (ELam p expr) s) e' ps'
renameExpr (Spanned (A.Let p e1 e2) s) = do
  e1' <- renameExpr e1
  push
  p' <- renamePattern p
  e2' <- renameExpr e2
  pop
  pure $ Spanned (ELet p' e1' e2') s
renameExpr (Spanned (A.Fn (Spanned n s) ps e1 e2) s') = do
  n' <- define n
  push
  ps' <- traverse renamePattern ps
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  pop
  pure $ Spanned (EFn (Spanned (n, n') s) ps' e1' e2') s'
renameExpr (Spanned (A.If c t e) s) = do
  c' <- renameExpr c
  t' <- renameExpr t
  e' <- renameExpr e
  pure $ Spanned (EIf c' t' e') s
renameExpr (Spanned (A.Unary op e) s) = do
  n <- lookupVar $ A.unOpName <$> op
  e' <- renameExpr e
  pure $ Spanned (EApp (Spanned (EVar (Spanned (A.unOpName (value op), n) s)) s) e') s
renameExpr (Spanned (A.Binary op e1 e2) s) = do
  n <- lookupVar $ A.binOpName <$> op
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  pure $ Spanned (EApp (Spanned (EApp (Spanned (EVar (Spanned (A.binOpName (value op), n) s)) s) e1') s) e2') s
renameExpr (Spanned (A.Match e cs) s) = do
  e' <- renameExpr e
  cs' <- traverse (\(p, e'') -> (,) <$> renamePattern p <*> renameExpr e'') cs
  pure $ Spanned (EMatch e' cs') s
renameExpr (Spanned (A.List es) s) = do
  es' <- traverse renameExpr es
  pure $ Spanned (EList es') s
renameExpr (Spanned (A.Array es) s) = do
  es' <- traverse renameExpr es
  pure $ Spanned (EArray es') s
renameExpr (Spanned (A.Tuple es) s) = do
  es' <- traverse renameExpr es
  pure $ Spanned (ETuple es') s
renameExpr (Spanned (A.Record n fs) s) = todo
renameExpr (Spanned A.Unit s) = pure $ Spanned EUnit s

renameTypeHint :: Spanned A.TypeHint -> ResState (Spanned TypeHint)
renameTypeHint (Spanned A.TypeHintInt s) = pure $ Spanned THInt s
renameTypeHint (Spanned A.TypeHintBool s) = pure $ Spanned THBool s
renameTypeHint (Spanned A.TypeHintString s) = pure $ Spanned THString s
renameTypeHint (Spanned (A.TypeHintVar v) s) = pure $ Spanned (THVar v) s
renameTypeHint (Spanned (A.TypeHintIdent n) s) = do
  n' <- lookupOrDefine (value n)
  pure $ Spanned (THIdent (Spanned (value n, n') s)) s
renameTypeHint (Spanned (A.TypeHintKind n ts) s) = do
  n' <- lookupOrDefine (value n)
  ts' <- traverse renameTypeHint ts
  pure $ Spanned (THKind (Spanned (value n, n') s) ts') s
renameTypeHint (Spanned (A.TypeHintList t) s) = do
  t' <- renameTypeHint t
  pure $ Spanned (THList t') s
renameTypeHint (Spanned (A.TypeHintArray t) s) = do
  t' <- renameTypeHint t
  pure $ Spanned (THArray t') s
renameTypeHint (Spanned (A.TypeHintTuple ts) s) = do
  ts' <- traverse renameTypeHint ts
  pure $ Spanned (THTuple ts') s
renameTypeHint (Spanned (A.TypeHintArrow t1 t2) s) = do
  t1' <- renameTypeHint t1
  t2' <- renameTypeHint t2
  pure $ Spanned (THArrow t1' t2') s
renameTypeHint (Spanned (A.TypeHintRecord n fs) s) = todo
renameTypeHint (Spanned A.TypeHintUnit s) = pure $ Spanned THUnit s

renamePattern :: Spanned A.Pattern -> ResState (Spanned Pattern)
renamePattern (Spanned A.PatternWildcard s) = pure $ Spanned PWildcard s
renamePattern (Spanned (A.PatternLit lit) s) = pure $ Spanned (PLit (renameLit lit)) s
renamePattern (Spanned (A.PatternVar (Spanned v s)) s') = do
  v' <- define v
  pure $ Spanned (PVar (Spanned (v, v') s)) s'
renamePattern (Spanned (A.PatternPair p1 p2) s) = do
  p1' <- renamePattern p1
  p2' <- renamePattern p2
  pure $ Spanned (PPair p1' p2') s
renamePattern (Spanned (A.PatternList ps) s) = do
  ps' <- traverse renamePattern ps
  pure $ Spanned (PList ps') s
renamePattern (Spanned A.PatternUnit s) = pure $ Spanned PUnit s

renameLit :: A.Lit -> Lit
renameLit (A.LitInt i) = LInt i
renameLit (A.LitBool b) = LBool b
renameLit (A.LitString t) = LString t
