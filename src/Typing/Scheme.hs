module Typing.Scheme (module Typing.Types, inst, applySubst, freeVars) where

import Control.Monad (forM)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Spanned
import Typing.Solver
import qualified Typing.Ty as Ty
import Typing.Types

inst :: Scheme -> InferState Ty
inst (Scheme vars t) = do
  vars' <- forM vars $ \(Spanned (TyVar _) sp) -> TVar <$> freshVar sp
  let s = Map.fromList $ zip vars vars'
  pure $ Ty.applySubst s t

applySubst :: Subst -> Scheme -> Scheme
applySubst s (Scheme vars t) = Scheme vars (Ty.applySubst s t)

freeVars :: Scheme -> Set (Spanned TyVar)
freeVars (Scheme vars t) = t & Ty.freeVars & Set.filter (`notElem` vars)
