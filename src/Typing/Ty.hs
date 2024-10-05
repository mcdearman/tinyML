module Typing.Ty (module Typing.Types, freeVars, applySubst, unify) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
import Spanned
import Text.Pretty.Simple
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

-- unify :: Ty -> Ty -> InferState ()
-- unify TInt TInt = pure ()
-- unify TBool TBool = pure ()
-- unify TChar TChar = pure ()
-- unify TString TString = pure ()
-- unify TUnit TUnit = pure ()
-- unify (TVar v1) t2 = bind v1 t2
-- unify t1 (TVar v2) = bind v2 t1
-- unify (TArrow t1 t2) (TArrow t1' t2') = do
--   unify t1 t1'
--   unify t2 t2'
-- unify (TList t1) (TList t2) = unify t1 t2
-- unify (TArray t1) (TArray t2) = unify t1 t2
-- unify (TTuple ts1) (TTuple ts2) = zipWithM_ unify ts1 ts2
-- unify t1 t2 = do
--   Solver.pushError $ UnificationError t1 t2

unify :: Ty -> Ty -> InferState ()
unify t1 t2 = do
  trace ("unify " ++ (unpack . toStrict $ pShow t1) ++ " and\n" ++ (unpack . toStrict $ pShow t2)) $ pure ()
  case (t1, t2) of
    (TInt, TInt) -> pure ()
    (TBool, TBool) -> pure ()
    (TChar, TChar) -> pure ()
    (TString, TString) -> pure ()
    (TUnit, TUnit) -> pure ()
    (TArrow tp tr, TArrow tp' tr') -> do
      unify tp tp'
      unify tr tr'
    (TVar v1, ty) -> bind v1 ty
    (ty, TVar v2) -> bind v2 ty
    (TList t, TList t') -> unify t t'
    (TArray t, TArray t') -> unify t t'
    (TTuple ts1, TTuple ts2) -> zipWithM_ unify ts1 ts2
    _ -> do
      trace "unification error" $ pure ()
      Solver.pushError $ UnificationError t1 t2

bind :: Spanned TyVar -> Ty -> InferState ()
bind v t
  | t == TVar v = pure ()
  | Set.member v (freeVars t) = do
      trace ("occurs check failed: " ++ (unpack . toStrict $ pShow v) ++ " in " ++ (unpack . toStrict $ pShow t)) $ pure ()
      Solver.pushError $ UnificationError (TVar v) t
  | otherwise = do
      s@Solver {subst = su} <- get
      trace ("bind " ++ show v ++ " to " ++ show t ++ " gives " ++ (show $ Map.insert v t su)) $ pure ()
      put s {subst = Map.insert v t su}
