module Typing.Infer where

import Control.Monad
import Control.Monad.State
import Control.Placeholder (todo)
import qualified Data.Map as Map
import qualified NIR as N
import Spanned
import Typing.Context
import qualified Typing.Context as Ctx
import Typing.Scheme
import Typing.TIR
import Typing.Ty

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
  p <- Ctx.lookup (snd (value n))
  v <- inst p
  pure $ Typed (Spanned (EVar n) s) v
genExprConstraints (Spanned (N.EApp e1 e2) s) = do
  e1'@(Typed _ t1) <- genExprConstraints e1
  e2'@(Typed _ t2) <- genExprConstraints e2
  v <- freshVar
  pushConstraint $ Eq t1 (TArrow t2 v)
  pure $ Typed (Spanned (EApp e1' e2') s) v
genExprConstraints (Spanned (N.ELam p e) s) = do
  push
  v <- freshVar
  p' <- genPatternConstraints p v False
  e'@(Typed _ te) <- genExprConstraints e
  pop
  pure $ Typed (Spanned (ELam p' e') s) (TArrow v te)
genExprConstraints e = todo

genPatternConstraints :: Spanned N.Pattern -> Ty -> Bool -> InferState (Typed Pattern)
genPatternConstraints (Spanned N.PWildcard s) _ _ = pure $ Typed (Spanned PWildcard s) TUnit
genPatternConstraints (Spanned (N.PLit (N.LInt i)) s) _ _ = pure $ Typed (Spanned (PLit (LInt i)) s) TInt
genPatternConstraints (Spanned (N.PLit (N.LBool b)) s) _ _ = pure $ Typed (Spanned (PLit (LBool b)) s) TBool
genPatternConstraints (Spanned (N.PLit (N.LString t)) s) _ _ = pure $ Typed (Spanned (PLit (LString t)) s) TString
genPatternConstraints (Spanned (N.PVar n) s) ty False = do
  define (snd (value n)) (Scheme [] ty)
  pure $ Typed (Spanned (PVar n) s) ty
genPatternConstraints (Spanned (N.PVar n) s) ty True = do
  p <- Ctx.lookup (snd (value n))
  v <- inst p
  pushConstraint $ Eq ty v
  pure $ Typed (Spanned (PVar n) s) v
genPatternConstraints (Spanned (N.PPair p1 p2) s) ty gen = do
  p1'@(Typed _ t1) <- genPatternConstraints p1 ty gen
  p2'@(Typed _ t2) <- genPatternConstraints p2 ty gen
  pushConstraint $ Eq (TList t1) t2
  pure $ Typed (Spanned (PPair p1' p2') s) t2
genPatternConstraints (Spanned (N.PList ps) s) ty gen = do
  ps' <- forM ps $ \p -> genPatternConstraints p ty gen
  pure $ Typed (Spanned (PList ps') s) ty
genPatternConstraints (Spanned N.PUnit s) _ _ = pure $ Typed (Spanned PUnit s) TUnit

inst :: Scheme -> InferState Ty
inst (Scheme vars t) = do
  vars' <- replicateM (length vars) freshVar
  let s = Map.fromList $ zip vars vars'
  pure $ applySubst s t

generalize :: Ty -> InferState Scheme
generalize t = do
  Solver {subst = s, ctx = c} <- get
  todo

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