module Ty where

import Common
import Data.Map
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Data.Text

data Ty
  = TInt
  | TBool
  | TChar
  | TString
  | TUnit
  | TVar (Spanned TyVar)
  | TArrow Ty Ty
  | TList Ty
  | TArray Ty
  | TTuple [Ty]
  | TRecord [(String, Ty)]
  | TCon String [Ty]
  deriving (Show, Eq, Ord)

freeVarsTy :: Ty -> Set (Spanned TyVar)
freeVarsTy (TVar v) = Set.singleton v
freeVarsTy (TArrow t1 t2) = freeVarsTy t1 `Set.union` freeVarsTy t2
freeVarsTy (TList t) = freeVarsTy t
freeVarsTy (TArray t) = freeVarsTy t
freeVarsTy (TTuple ts) = Set.unions $ fmap freeVarsTy ts
freeVarsTy _ = Set.empty

applySubstTy :: Subst -> Ty -> Ty
applySubstTy s ty@(TVar v) = Map.findWithDefault ty v s
applySubstTy s (TArrow t1 t2) = TArrow (applySubstTy s t1) (applySubstTy s t2)
applySubstTy s (TList t) = TList (applySubstTy s t)
applySubstTy s (TArray t) = TArray (applySubstTy s t)
applySubstTy s (TTuple ts) = TTuple (fmap (applySubstTy s) ts)
applySubstTy _ t = t

type Subst = Map (Spanned TyVar) Ty

newtype TyVar = TyVar Unique deriving (Show, Eq, Ord)

instance Pretty TyVar where
  pretty (TyVar (Id i)) = pack $ "t" ++ show i

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)