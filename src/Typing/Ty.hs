module Typing.Ty where

import Data.Map
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Spanned
import Unique

data Ty
  = TInt
  | TBool
  | TChar
  | TString
  | TUnit
  | TVar TyVar
  | TArrow Ty Ty
  | TList Ty
  | TArray Ty
  | TTuple [Ty]
  | TRecord [(String, Ty)]
  | TCon String [Ty]
  deriving (Show, Eq, Ord)

freeVars :: Ty -> Set TyVar
freeVars (TVar v) = Set.singleton v
freeVars (TArrow t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (TList t) = freeVars t
freeVars (TArray t) = freeVars t
freeVars (TTuple ts) = Set.unions $ fmap freeVars ts
freeVars _ = Set.empty

type Subst = Map TyVar Ty

applySubst :: Subst -> Ty -> Ty
applySubst s (TVar v) = Map.findWithDefault (TVar v) v s
applySubst s (TArrow t1 t2) = TArrow (applySubst s t1) (applySubst s t2)
applySubst s (TList t) = TList (applySubst s t)
applySubst s (TArray t) = TArray (applySubst s t)
applySubst s (TTuple ts) = TTuple (fmap (applySubst s) ts)
applySubst _ t = t

newtype TyVar = TyVar Unique deriving (Show, Eq, Ord)

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)