module Typing.Context where

import Control.Monad.State
import Data.Map
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Typing.Scheme
import Typing.Ty
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

freshVar :: InferState Ty
freshVar = do
  s@Solver {tyVarCounter = c@(Id v)} <- get
  put s {tyVarCounter = Id (v + 1)}
  pure $ TVar (TyVar c)

pushConstraint :: Constraint -> InferState ()
pushConstraint c = modify' $ \s@Solver {constraints = cs} -> s {constraints = c : cs}

pushError :: InferError -> InferState ()
pushError e = modify' $ \s@Solver {errors = es} -> s {errors = e : es}

newtype Context = Context [Map Unique Scheme] deriving (Show)

defaultCtx :: Context
defaultCtx = Context []

freeVarsCtx :: Context -> Set TyVar
freeVarsCtx (Context fs) = Set.unions $ fmap freeVarsMap fs
  where
    freeVarsMap = Set.unions . fmap freeVarsScheme . Map.elems
    freeVarsScheme (Scheme vars t) = freeVars t `Set.difference` Set.fromList vars

pop :: InferState ()
pop = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  case fs of
    [] -> error "cannot pop empty context"
    _ : [] -> error "cannot pop top-level context"
    _ : fs' -> put s {ctx = Context fs'}

push :: InferState ()
push = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  put s {ctx = Context $ Map.empty : fs}

define :: Unique -> Scheme -> InferState ()
define n scm = do
  s@Solver {ctx = Context fs} <- get
  case fs of
    [] -> do
      let f = Map.singleton n scm
      put s {ctx = Context [f]}
    (f : fs') -> put s {ctx = Context $ Map.insert n scm f : fs'}
  pure ()

lookup :: Unique -> InferState Scheme
lookup n = do
  Solver {ctx = Context fs} <- get
  case lookup' n fs of
    Just s -> pure s
    Nothing -> error "unbound variable"
  where
    lookup' _ [] = Nothing
    lookup' n' (m : ms) = case Map.lookup n m of
      Just s -> Just s
      Nothing -> lookup' n' ms