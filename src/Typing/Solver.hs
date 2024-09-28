module Typing.Solver (module Typing.Types, freshVar, pushConstraint, pushError) where

import Control.Monad.State
import Typing.Types
import Unique

freshVar :: InferState Ty
freshVar = do
  s@Solver {tyVarCounter = c@(Id v)} <- get
  put s {tyVarCounter = Id (v + 1)}
  pure $ TVar (TyVar c)

pushConstraint :: Constraint -> InferState ()
pushConstraint c = modify' $ \s@Solver {constraints = cs} -> s {constraints = c : cs}

pushError :: InferError -> InferState ()
pushError e = modify' $ \s@Solver {errors = es} -> s {errors = e : es}