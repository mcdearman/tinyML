module Infer where

import Control.Monad.State
import Control.Placeholder (todo)
import Data.Map (Map)
import Data.Text
import qualified NIR as N
import Spanned
import Ty

data Solver = Solver
  { constraints :: [Constraint],
    subst :: Subst,
    ctx :: Context
  }

data Constraint = Eq Ty Ty

type Subst = Map TyVar Ty

newtype Context = Context [Map Text Scheme]

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
