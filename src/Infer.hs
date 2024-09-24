module Infer where

import Control.Applicative
import Control.Monad
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

data InferError = UnificationError Ty Ty

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

freshVar :: InferState Ty
freshVar = do
  s@Solver {tyVarCounter = c@(Id v)} <- get
  put s {tyVarCounter = Id (v + 1)}
  pure $ TVar (TyVar c)

applySubst :: Subst -> Ty -> Ty
applySubst s (TVar v) = Map.findWithDefault (TVar v) v s
applySubst s (TArrow t1 t2) = TArrow (applySubst s t1) (applySubst s t2)
applySubst s (TList t) = TList (applySubst s t)
applySubst s (TArray t) = TArray (applySubst s t)
applySubst s (TTuple ts) = TTuple (fmap (applySubst s) ts)
applySubst _ t = t

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
genExprConstraints (Spanned (N.ELit (N.LInt i)) s) = pure $ Typed (Spanned (ELit (LInt i)) s) TInt
genExprConstraints (Spanned (N.ELit (N.LBool b)) s) = pure $ Typed (Spanned (ELit (LBool b)) s) TBool
genExprConstraints (Spanned (N.ELit (N.LString t)) s) = pure $ Typed (Spanned (ELit (LString t)) s) TString
genExprConstraints (Spanned (N.EVar n) s) = do
  v <- lookupCtx (snd (value n))
  x <- inst v
  pure $ Typed (Spanned (EVar n) s) x
genExprConstraints e = todo

inst :: Scheme -> InferState Ty
inst (Scheme vars t) = do
  vars' <- replicateM (length vars) freshVar
  let s = Map.fromList $ zip vars vars'
  pure $ applySubst s t

unify :: Ty -> Ty -> InferState ()
unify TInt TInt = pure ()
unify TBool TBool = pure ()
unify TChar TChar = pure ()
unify TString TString = pure ()
unify TUnit TUnit = pure ()
unify (TVar v1) t2 = bind v1 t2
unify t1 (TVar v2) = bind v2 t1
unify (TArrow t1 t2) (TArrow t1' t2') = do
  unify t1 t1'
  unify t2 t2'
unify (TList t1) (TList t2) = unify t1 t2
unify (TArray t1) (TArray t2) = unify t1 t2
unify (TTuple ts1) (TTuple ts2) = zipWithM_ unify ts1 ts2
unify t1 t2 = do
  pushError $ UnificationError t1 t2

bind :: TyVar -> Ty -> InferState ()
bind v t = do
  s@Solver {subst = su} <- get
  put s {subst = Map.insert v t su}