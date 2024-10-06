module Typing.Solver (module Typing.Types, defaultSolver, freshVar, pushConstraint, pushError) where

import Control.Monad.State.Strict
import Data.Function ((&))
import qualified Data.Map as Map
import Span
import Spanned
import Typing.Types
import Unique

builtins :: InferState ()
builtins = do
  let m =
        Map.empty
          & Map.insert (Id 0) (Scheme [] $ TArrow TInt TInt)
          & Map.insert (Id 1) (Scheme [] $ TArrow TBool TBool)
          & Map.insert (Id 2) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 3) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 4) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 5) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 6) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 7) (Scheme [] $ TArrow TInt (TArrow TInt TInt))
          & Map.insert (Id 10) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 11) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 12) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
          & Map.insert (Id 13) (Scheme [] $ TArrow TInt (TArrow TInt TBool))
  v1 <- freshVar $ NoLoc
  let m1 = Map.insert (Id 8) (Scheme [v1] $ TArrow (TVar v1) (TVar v1)) m
  v2 <- freshVar $ NoLoc
  let m2 = Map.insert (Id 9) (Scheme [v2] $ TArrow (TVar v2) (TVar v2)) m1
  v3 <- freshVar $ NoLoc
  let m3 = Map.insert (Id 14) (Scheme [v3] $ TArrow (TVar v3) (TArrow (TList (TVar v3)) (TList (TVar v3)))) m2
  v4 <- freshVar $ NoLoc
  v5 <- freshVar $ NoLoc
  let m4 =
        Map.insert
          (Id 15)
          (Scheme [v4, v5] $ TArrow (TVar v4) (TArrow (TArrow (TVar v4) (TVar v5)) (TVar v5)))
          m3
  s <- get
  case ctx s of
    Context [] -> put s {ctx = Context $ [m4]}
    _ -> error "builtins: context is not empty"

defaultSolver :: Solver
defaultSolver =
  let s =
        Solver
          { constraints = [],
            tyVarCounter = Id 0,
            subst = Map.empty,
            ctx = Context [],
            errors = []
          }
   in execState builtins s

freshVar :: Span -> InferState (Spanned TyVar)
freshVar sp = do
  s@Solver {tyVarCounter = c@(Id v)} <- get
  put s {tyVarCounter = Id (v + 1)}
  pure $ Spanned (TyVar c) sp

pushConstraint :: Constraint -> InferState ()
pushConstraint c = modify' $ \s@Solver {constraints = cs} -> s {constraints = c : cs}

pushError :: InferError -> InferState ()
pushError e = modify' $ \s@Solver {errors = es} -> s {errors = e : es}