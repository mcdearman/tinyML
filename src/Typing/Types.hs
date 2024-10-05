module Typing.Types where

import Control.Monad.State
import Data.Map
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

type Subst = Map (Spanned TyVar) Ty

newtype TyVar = TyVar Unique deriving (Show, Eq, Ord)

data Typed a = Typed (Spanned a) Ty deriving (Show, Eq)

newtype Context = Context [Map Unique Scheme] deriving (Show)

data Scheme = Scheme [Spanned TyVar] Ty deriving (Show)