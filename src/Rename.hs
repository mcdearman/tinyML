module Rename where

import qualified AST as A
import Control.Monad.State
import Data.Text (Text)
import NIR
import Span
import Spanned
import Unique
import Prelude hiding (span)

newtype RenameError = UnboundVariable Span deriving (Show, Eq)

newtype Env = Env [Frame] deriving (Show, Eq)

instance Semigroup Env where
  Env fs <> Env fs' = Env (fs <> fs')

type Frame = [(Text, ResId)]

data Resolver = Resolver
  { resId :: ResId,
    env :: Env,
    errors :: [RenameError]
  }
  deriving (Show, Eq)

type ResState a = State Resolver a

defaultEnv :: Env
defaultEnv = Env []

push :: ResState Env
push = do
  r@Resolver {env = Env fs} <- get
  put r {env = Env $ [] : fs}
  pure $ env r

pop :: ResState Env
pop = do
  r@Resolver {env = Env fs} <- get
  case fs of
    [] -> error "cannot pop empty environment"
    _ : fs' -> do
      put r {env = Env fs'}
      pure $ env r

define :: Text -> ResState ResId
define n = do
  r@Resolver {resId = i, env = Env fs, errors = _} <- get
  let Id i' = i
  put r {resId = Id $ i' + 1, env = Env $ [(n, i)] : fs}
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
lookup' n' (Env (f : fs)) = case lookup n' f of
  Just i -> Just i
  Nothing -> lookup' n' (Env fs)

pushError :: RenameError -> ResState ()
pushError e = do
  r@Resolver {errors = es} <- get
  put r {errors = e : es}

renameProgram :: Spanned A.Program -> ResState (Spanned Program)
renameProgram (Spanned (A.PFile name m) s) = do
  m' <- renameModule m
  pure $ Spanned (PFile name m') s
renameProgram (Spanned (A.PRepl (Left d)) s) = do
  d' <- renameDecl d
  pure $ Spanned (PRepl (Left d')) s
renameProgram (Spanned (A.PRepl (Right e)) s) = do
  e' <- renameExpr e
  pure $ Spanned (PRepl (Right e')) s

renameModule :: Spanned A.Module -> ResState (Spanned Module)
renameModule (Spanned (A.Module (Spanned n s) ds) s') = do
  n' <- lookupOrDefine n
  ds' <- traverse renameDecl ds
  pure $ Spanned (Module (Spanned n' s) ds') s'

renameDecl :: Spanned A.Decl -> ResState (Spanned Decl)
renameDecl (Spanned (A.DDef p e) s) = do
  p' <- renamePattern p
  e' <- renameExpr e
  pure $ Spanned (DDef p' e') s
renameDecl (Spanned (A.DFn (Spanned n s) ps e) s') = do
  n' <- define n
  push
  ps' <- traverse renamePattern ps
  e' <- renameExpr e
  pop
  pure $ Spanned (DFn (Spanned n' s) ps' e') s'
renameDecl _ = undefined

renameExpr :: Spanned A.Expr -> ResState (Spanned Expr)
renameExpr (Spanned (A.ELit lit) s) = pure $ Spanned (ELit (renameLit lit)) s
renameExpr (Spanned (A.EVar v) s) = do
  v' <- lookupVar v
  pure $ Spanned (EVar (Spanned v' s)) s
renameExpr (Spanned (A.EApp f arg) s) = do
  f' <- renameExpr f
  arg' <- renameExpr arg
  pure $ Spanned (EApp f' arg') s
renameExpr (Spanned (A.ELam ps e) s) = do
  push
  ps' <- traverse renamePattern ps
  e' <- renameExpr e
  pop
  pure $ Spanned (ELam ps' e') s
renameExpr (Spanned (A.ELet p e1 e2) s) = do
  e1' <- renameExpr e1
  push
  p' <- renamePattern p
  e2' <- renameExpr e2
  pop
  pure $ Spanned (ELet p' e1' e2') s
renameExpr (Spanned (A.EFn (Spanned n s) ps e1 e2) s') = do
  n' <- define n
  push
  ps' <- traverse renamePattern ps
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  pop
  pure $ Spanned (EFn (Spanned n' s) ps' e1' e2') s'
renameExpr (Spanned (A.EIf c t e) s) = do
  c' <- renameExpr c
  t' <- renameExpr t
  e' <- renameExpr e
  pure $ Spanned (EIf c' t' e') s
renameExpr (Spanned (A.EUnary op e) s) = do
  n <- lookupOrDefine $ A.unOpName (value op)
  e' <- renameExpr e
  pure $ Spanned (EApp (Spanned (EVar (Spanned n s)) s) e') s
renameExpr (Spanned (A.EBinary op e1 e2) s) = do
  n <- lookupOrDefine $ A.binOpName (value op)
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  pure $ Spanned (EApp (Spanned (EApp (Spanned (EVar (Spanned n s)) s) e1') s) e2') s
renameExpr (Spanned (A.EMatch e cs) s) = do
  e' <- renameExpr e
  cs' <- traverse (\(p, e'') -> (,) <$> renamePattern p <*> renameExpr e'') cs
  pure $ Spanned (EMatch e' cs') s
renameExpr (Spanned (A.EList es) s) = do
  es' <- traverse renameExpr es
  pure $ Spanned (EList es') s
renameExpr (Spanned (A.EArray es) s) = do
  es' <- traverse renameExpr es
  pure $ Spanned (EArray es') s
renameExpr (Spanned (A.ETuple es) s) = do
  es' <- traverse renameExpr es
  pure $ Spanned (ETuple es') s
renameExpr (Spanned (A.ERecord n fs) s) = undefined
renameExpr (Spanned A.EUnit s) = pure $ Spanned EUnit s

renamePattern :: Spanned A.Pattern -> ResState (Spanned Pattern)
renamePattern (Spanned (A.PVar (Spanned v s)) s') = do
  v' <- define v
  pure $ Spanned (PVar (Spanned v' s)) s'
renamePattern _ = undefined

renameLit :: Spanned A.Lit -> Spanned Lit
renameLit (Spanned (A.LInt i) s) = Spanned (LInt i) s
renameLit (Spanned (A.LBool b) s) = Spanned (LBool b) s
renameLit (Spanned (A.LString c) s) = Spanned (LString c) s