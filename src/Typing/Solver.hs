module Typing.Solver (module Typing.Types, defaultSolver, freshVar, pushConstraint, pushError) where

import Control.Monad.State
import Data.Map
import qualified Data.Map as Map
import NIR (Name)
import Typing.Types
import Unique

defaultSolver :: Solver
defaultSolver =
  Solver
    { constraints = [],
      tyVarCounter = Id 0,
      subst = Map.empty,
      ctx = Context [],
      errors = []
    }

freshVar :: InferState TyVar
freshVar = do
  s@Solver {tyVarCounter = c@(Id v)} <- get
  put s {tyVarCounter = Id (v + 1)}
  pure $ TyVar c

pushConstraint :: Constraint -> InferState ()
pushConstraint c = modify' $ \s@Solver {constraints = cs} -> s {constraints = c : cs}

pushError :: InferError -> InferState ()
pushError e = modify' $ \s@Solver {errors = es} -> s {errors = e : es}