module Typing.Scheme (module Typing.Types, inst) where

import Control.Monad (replicateM)
import qualified Data.Map as Map
import Typing.Solver
import qualified Typing.Ty as Ty
import Typing.Types

inst :: Scheme -> InferState Ty
inst (Scheme vars t) = do
  vars' <- replicateM (length vars) freshVar
  let s = Map.fromList $ zip vars vars'
  pure $ Ty.applySubst s t