module TMC.Ty where

import Common
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

freeVars :: Ty -> Set (Spanned TyVar)
freeVars (TyVar v) = Set.singleton v
freeVars (TyArrow t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (TyList t) = freeVars t
freeVars (TyArray t) = freeVars t
freeVars (TyTuple ts) = Set.unions $ fmap freeVars ts
freeVars _ = Set.empty

applySubst :: Subst -> Ty -> Ty
applySubst s ty@(TyVar v) = Map.findWithDefault ty v s
applySubst s (TyArrow t1 t2) = TyArrow (applySubst s t1) (applySubst s t2)
applySubst s (TyList t) = TyList (applySubst s t)
applySubst s (TyArray t) = TyArray (applySubst s t)
applySubst s (TyTuple ts) = TyTuple (fmap (applySubst s) ts)
applySubst _ t = t

type Subst = Map (Spanned TyVar) Ty

newtype TyVar = VarId Unique deriving (Show, Eq, Ord)

instance Pretty TyVar where
  pretty (VarId (Id i)) = pack $ "t" ++ show i

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)
