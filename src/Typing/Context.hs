module Typing.Context (module Typing.Types, freeVars, pop, push, define, lookup) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Typing.Scheme
import qualified Typing.Ty as Ty
import Typing.Types
import Unique
import Prelude hiding (lookup)

defaultCtx :: Context
defaultCtx = Context []

freeVars :: Context -> Set TyVar
freeVars (Context fs) = Set.unions $ fmap freeVarsMap fs
  where
    freeVarsMap = Set.unions . fmap freeVarsScheme . Map.elems
    freeVarsScheme (Scheme vars t) = Ty.freeVars t `Set.difference` Set.fromList vars

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