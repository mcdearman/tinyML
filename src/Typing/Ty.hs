module Typing.Ty (module Typing.Types, freeVars, applySubst, unify) where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Data.Text
import Debug.Trace (trace, traceM)
import Pretty
import Spanned
import Typing.Solver as Solver
import Typing.Types

freeVars :: Ty -> Set (Spanned TyVar)
freeVars (TVar v) = Set.singleton v
freeVars (TArrow t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (TList t) = freeVars t
freeVars (TArray t) = freeVars t
freeVars (TTuple ts) = Set.unions $ fmap freeVars ts
freeVars _ = Set.empty

applySubst :: Subst -> Ty -> Ty
applySubst s ty@(TVar v) = Map.findWithDefault ty v s
applySubst s (TArrow t1 t2) = TArrow (applySubst s t1) (applySubst s t2)
applySubst s (TList t) = TList (applySubst s t)
applySubst s (TArray t) = TArray (applySubst s t)
applySubst s (TTuple ts) = TTuple (fmap (applySubst s) ts)
applySubst _ t = t

unify :: Ty -> Ty -> InferState ()
unify t1 t2 = do
  trace ("unify " ++ (unpack $ pretty t1) ++ " and " ++ (unpack $ pretty t2)) $ pure ()
  case (t1, t2) of
    (TInt, TInt) -> pure ()
    (TBool, TBool) -> pure ()
    (TChar, TChar) -> pure ()
    (TString, TString) -> pure ()
    (TUnit, TUnit) -> pure ()
    (TArrow tp tr, TArrow tp' tr') -> do
      unify tp tp'
      unify tr tr'
    (TVar v1, _) -> bind v1 t2
    (_, TVar v2) -> bind v2 t1
    (TList t, TList t') -> unify t t'
    (TArray t, TArray t') -> unify t t'
    (TTuple ts1, TTuple ts2) -> zipWithM_ unify ts1 ts2
    _ -> do
      Solver.pushError $ UnificationError t1 t2

bind :: Spanned TyVar -> Ty -> InferState ()
bind v t
  | t == TVar v = pure ()
  | Set.member v (freeVars t) = do
      Solver.pushError $ Occurs (TVar v) t
  | otherwise = do
      s <- get
      let su = subst s
      -- traceM ("bind " ++ (unpack $ pretty v) ++ " to " ++ (unpack $ pretty t) ++ " gives " ++ (show $ Map.insert v t su))
      put s {subst = Map.insert v t su}