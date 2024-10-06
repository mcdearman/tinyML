module Typing.Types where

import Control.Monad.State.Strict
import Data.Map
import Data.Text (pack, unpack)
import Pretty
import Spanned
import Unique

data InferError = UnificationError Ty Ty deriving (Show, Eq)

data Solver = Solver
  { constraints :: [Constraint],
    tyVarCounter :: Unique,
    subst :: Subst,
    ctx :: Context,
    errors :: [InferError]
  }
  deriving (Show)

data Constraint = Eq Ty Ty deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Eq t1 t2) = pretty t1 <> " = " <> pretty t2

type InferState a = State Solver a

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
  pretty (TArray t) = pack $ "Array " ++ unpack (pretty t)
  pretty (TTuple ts) = pack $ "(" ++ unwords (fmap show ts) ++ ")"
  pretty (TRecord fs) = pack $ "{" ++ unwords (fmap (\(n, t) -> n ++ ": " ++ show t) fs) ++ "}"
  pretty (TCon n ts) = pack $ n ++ " " ++ unwords (fmap show ts)

type Subst = Map (Spanned TyVar) Ty

newtype TyVar = TyVar Unique deriving (Show, Eq, Ord)

instance Pretty TyVar where
  pretty (TyVar (Id i)) = pack $ "t" ++ show i

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)

newtype Context = Context [Map Unique Scheme] deriving (Show)

data Scheme = Scheme [Spanned TyVar] Ty deriving (Show)