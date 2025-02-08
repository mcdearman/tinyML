module Ty where

import Common
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Data.Text (pack, unpack)

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

instance Pretty Ty where
  pretty TInt = "Int"
  pretty TBool = "Bool"
  pretty TChar = "Char"
  pretty TString = "String"
  pretty TUnit = "Unit"
  pretty (TVar v) = pretty v
  pretty (TArrow t1 t2) = case t1 of
    TArrow _ _ -> pack $ "(" ++ unpack (pretty t1) ++ ") -> " ++ unpack (pretty t2)
    _ -> pack $ unpack (pretty t1) ++ " -> " ++ unpack (pretty t2)
  pretty (TList t) = pack $ "[" ++ unpack (pretty t) ++ "]"
  pretty (TArray t) = pack $ "#[" ++ unpack (pretty t) ++ "]"
  pretty (TTuple ts) = pack $ "(" ++ unwords (fmap show ts) ++ ")"
  pretty (TRecord fs) = pack $ "{" ++ unwords (fmap (\(n, t) -> n ++ ": " ++ show t) fs) ++ "}"
  pretty (TCon n ts) = pack $ n ++ " " ++ unwords (fmap show ts)

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

data Scheme = Scheme [Spanned TyVar] Ty deriving (Show)

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Scheme vars t) = Scheme vars (applySubstTy s t)

freeVarsScheme :: Scheme -> Set (Spanned TyVar)
freeVarsScheme (Scheme vars t) = t & freeVarsTy & Set.filter (`notElem` vars)