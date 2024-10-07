module Typing.Constraint where

import qualified Typing.Ty as Ty
import Typing.Types

applySubst :: Subst -> Constraint -> Constraint
applySubst s (Eq t1 t2) = Eq (Ty.applySubst s t1) (Ty.applySubst s t2)