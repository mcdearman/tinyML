module Scheme where

import Common (Spanned)
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import Ty (Subst, Ty, TyVar)
import qualified Ty as Ty

data Scheme = Scheme [Spanned TyVar] Ty deriving (Show)

applySubst :: Subst -> Scheme -> Scheme
applySubst s (Scheme vars t) = Scheme vars (Ty.applySubst s t)

freeVars :: Scheme -> Set (Spanned TyVar)
freeVars (Scheme vars t) = t & Ty.freeVars & Set.filter (`notElem` vars)