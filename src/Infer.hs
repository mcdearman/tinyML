module Infer where

import Control.Applicative
import Control.Monad.State
import Control.Placeholder (todo)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified NIR as N
import Spanned
import Ty

data InferError = UnificationError (Spanned Ty) (Spanned Ty)

data Solver = Solver
  { constraints :: [Constraint],
    subst :: Subst,
    ctx :: Context,
    errors :: [InferError]
  }

data Constraint = Eq Ty Ty

type Subst = Map TyVar Ty

newtype Context = Context [Map Text Scheme]

defaultCtx :: Context
defaultCtx = Context []

pop :: InferState ()
pop = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  case fs of
    [] -> error "cannot pop empty context"
    _ : fs' -> put s {ctx = Context fs'}

push :: InferState ()
push = do
  s@Solver {constraints = _, subst = _, ctx = Context fs, errors = _} <- get
  put s {ctx = Context $ Map.empty : fs}

define :: Text -> Scheme -> InferState ()
define n s = do
  modify' $ \s'@Solver {ctx = Context fs} -> s' {ctx = Context $ Map.insert n s (head fs) : tail fs}

lookupCtx :: Text -> InferState Scheme
lookupCtx n = do
  Solver {ctx = Context fs} <- get
  case lookup' n fs of
    Just s -> pure s
    Nothing -> error "unbound variable"
  where
    lookup' _ [] = Nothing
    lookup' n' (m : ms) = case Map.lookup n m of
      Just s -> Just s
      Nothing -> lookup' n' ms

data Scheme = Scheme [TyVar] Ty

type InferState a = State Solver a

genConstraints :: Spanned N.Program -> InferState ()
genConstraints p = todo

genModuleConstraints :: Spanned N.Module -> InferState ()
genModuleConstraints m = todo

genDeclConstraints :: Spanned N.Decl -> InferState ()
genDeclConstraints d = todo

genExprConstraints :: Spanned N.Expr -> InferState ()
genExprConstraints e = todo
