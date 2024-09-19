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
    [] -> error "pop: empty stack"
    _ : fs' -> do
      put r {env = Env fs'}
      pure $ env r

define :: Text -> ResState ResId
define n = do
  r@Resolver {resId = i, env = Env fs, errors = _} <- get
  let Id i' = i
  put r {resId = Id $ i' + 1, env = Env $ [(n, i)] : fs}
  pure i

lookupVar :: A.Name -> ResState ResId
lookupVar n = do
  Resolver {env = e} <- get
  case lookup' (value n) e of
    Just i -> pure i
    Nothing -> do
      pushError $ UnboundVariable (span n)
      pure $ Id 0
  where
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
renameProgram (Spanned (A.PRepl m) s) = do
  m' <- renameModule m
  pure $ Spanned (PRepl m') s

renameModule :: Spanned A.Module -> ResState (Spanned Module)
renameModule (Spanned (A.Module (Spanned n s) ds) s') = do
  n' <- define n
  ds' <- traverse renameDecl ds
  pure $ Spanned (Module (Spanned n' s) ds') s'

renameDecl :: Spanned A.Decl -> ResState (Spanned Decl)
renameDecl (Spanned (A.DDef p e) s) = do
  p' <- renamePattern p
  e' <- renameExpr e
  pure $ Spanned (DDef p' e') s
renameDecl (Spanned (A.DFn (Spanned n s) ps e) s') = do
  n' <- define n
  ps' <- traverse renamePattern ps
  e' <- renameExpr e
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
renameExpr _ = undefined

renamePattern :: Spanned A.Pattern -> ResState (Spanned Pattern)
renamePattern (Spanned (A.PVar (Spanned v s)) s') = do
  v' <- define v
  pure $ Spanned (PVar (Spanned v' s)) s'
renamePattern _ = undefined

renameLit :: Spanned A.Lit -> Spanned Lit
renameLit (Spanned (A.LInt i) s) = Spanned (LInt i) s
renameLit (Spanned (A.LBool b) s) = Spanned (LBool b) s
renameLit (Spanned (A.LString c) s) = Spanned (LString c) s