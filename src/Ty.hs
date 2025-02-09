module Ty where

import Common
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Data.Text (pack, unpack)

data Ty
  = TyInt
  | TyBool
  | TyChar
  | TyString
  | TyUnit
  | TyVar (Spanned TyVar)
  | TyArrow Ty Ty
  | TyList Ty
  | TyArray Ty
  | TyTuple [Ty]
  | TyRecord [(String, Ty)]
  | TyCon String [Ty]
  deriving (Show, Eq, Ord)

instance Pretty Ty where
  pretty TyInt = "Int"
  pretty TyBool = "Bool"
  pretty TyChar = "Char"
  pretty TyString = "String"
  pretty TyUnit = "Unit"
  pretty (TyVar v) = pretty v
  pretty (TyArrow t1 t2) = case t1 of
    TyArrow _ _ -> pack $ "(" ++ unpack (pretty t1) ++ ") -> " ++ unpack (pretty t2)
    _ -> pack $ unpack (pretty t1) ++ " -> " ++ unpack (pretty t2)
  pretty (TyList t) = pack $ "[" ++ unpack (pretty t) ++ "]"
  pretty (TyArray t) = pack $ "#[" ++ unpack (pretty t) ++ "]"
  pretty (TyTuple ts) = pack $ "(" ++ unwords (fmap show ts) ++ ")"
  pretty (TyRecord fs) = pack $ "{" ++ unwords (fmap (\(n, t) -> n ++ ": " ++ show t) fs) ++ "}"
  pretty (TyCon n ts) = pack $ n ++ " " ++ unwords (fmap show ts)

freeVarsTy :: Ty -> Set (Spanned TyVar)
freeVarsTy (TyVar v) = Set.singleton v
freeVarsTy (TyArrow t1 t2) = freeVarsTy t1 `Set.union` freeVarsTy t2
freeVarsTy (TyList t) = freeVarsTy t
freeVarsTy (TyArray t) = freeVarsTy t
freeVarsTy (TyTuple ts) = Set.unions $ fmap freeVarsTy ts
freeVarsTy _ = Set.empty

applySubstTy :: Subst -> Ty -> Ty
applySubstTy s ty@(TyVar v) = Map.findWithDefault ty v s
applySubstTy s (TyArrow t1 t2) = TyArrow (applySubstTy s t1) (applySubstTy s t2)
applySubstTy s (TyList t) = TyList (applySubstTy s t)
applySubstTy s (TyArray t) = TyArray (applySubstTy s t)
applySubstTy s (TyTuple ts) = TyTuple (fmap (applySubstTy s) ts)
applySubstTy _ t = t

type Subst = Map (Spanned TyVar) Ty

newtype TyVar = VarId Unique deriving (Show, Eq, Ord)

instance Pretty TyVar where
  pretty (VarId (Id i)) = pack $ "t" ++ show i

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)

data Scheme = Scheme [Spanned TyVar] Ty deriving (Show)

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Scheme vars t) = Scheme vars (applySubstTy s t)

freeVarsScheme :: Scheme -> Set (Spanned TyVar)
freeVarsScheme (Scheme vars t) = t & freeVarsTy & Set.filter (`notElem` vars)