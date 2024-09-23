module Infer where

import Control.Applicative
import Control.Monad.State
import Control.Placeholder (todo)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified NIR as N
import Spanned
import TIR
import Ty
import Unique

data InferError = UnificationError (Spanned Ty) (Spanned Ty)

data Solver = Solver
  { constraints :: [Constraint],
    tyVarCounter :: Unique,
    subst :: Subst,
    ctx :: Context,
    errors :: [InferError]
  }

data Constraint = Eq Ty Ty

type Subst = Map TyVar Ty

newtype Context = Context [Map Unique Scheme]

defaultCtx :: Context
defaultCtx = Context []

pop :: InferState ()
pop = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  case fs of
    [] -> error "cannot pop empty context"
    _ : fs' -> put s {ctx = Context fs'}

push :: InferState ()
push = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  put s {ctx = Context $ Map.empty : fs}

define :: Unique -> Scheme -> InferState ()
define n s = do
  modify' $ \s'@Solver {ctx = Context fs} -> s' {ctx = Context $ Map.insert n s (head fs) : tail fs}

lookupCtx :: Unique -> InferState Scheme
lookupCtx n = do
  Solver {ctx = Context fs} <- get
  case lookup' n fs of
    Just s -> pure s
    Nothing -> error "unbound variable"
  where
    lookup' _ [] = Nothing
    lookup' n' (m : ms) = case Map.lookup n m of
      Just s -> Just s
      Nothing -> lookup' n' ms

pushError :: InferError -> InferState ()
pushError e = modify' $ \s@Solver {errors = es} -> s {errors = e : es}

data Scheme = Scheme [TyVar] Ty

type InferState a = State Solver a

genConstraints :: Spanned N.Program -> InferState ()
genConstraints p = todo

genModuleConstraints :: Spanned N.Module -> InferState ()
genModuleConstraints m = todo

genDeclConstraints :: Spanned N.Decl -> InferState ()
genDeclConstraints d = todo

genExprConstraints :: Spanned N.Expr -> InferState (Typed Expr)
-- genExprConstraints (Spanned (N.ELit l) s) = pure $ Spanned (ELit (genLitConstraints l)) s
-- genExprConstraints (Spanned (N.EVar n) _) = do
--   v <- lookupCtx (snd (value n))
--   pure ()
genExprConstraints e = todo

genLitConstraints :: Spanned N.Lit -> Spanned Lit
genLitConstraints (Spanned (N.LInt i) s) = Spanned (LInt i) s
genLitConstraints (Spanned (N.LBool b) s) = Spanned (LBool b) s
genLitConstraints (Spanned (N.LString st) s) = Spanned (LString st) s

unify :: Spanned Ty -> Spanned Ty -> InferState ()
unify (Spanned TInt _) (Spanned TInt _) = pure ()
unify (Spanned TBool _) (Spanned TBool _) = pure ()
unify (Spanned TChar _) (Spanned TChar _) = pure ()
unify (Spanned TString _) (Spanned TString _) = pure ()
unify (Spanned TUnit _) (Spanned TUnit _) = pure ()
unify (Spanned (TVar v1) _) t2 = bind v1 t2
unify t1 (Spanned (TVar v2) _) = bind v2 t1
unify t1 t2 = do
  pushError $ UnificationError t1 t2

bind :: TyVar -> Spanned Ty -> InferState ()
bind v t = do
  s@Solver {subst = su} <- get
  put s {subst = Map.insert v (value t) su}