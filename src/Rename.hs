module Rename where

import qualified AST as A
import Control.Monad.State
import Data.Text (Text)
import NIR
import Span
import Spanned

newtype RenameError = UnboundVariable Span deriving (Show, Eq)

data RenameResult a = Ok a | RenameError deriving (Show, Eq)

type Rename a = State Env (RenameResult a)

data Env = Env [Frame] Int deriving (Show, Eq)

newtype Frame = Frame [(Text, ResId)] deriving (Show, Eq)

defaultEnv :: Env
defaultEnv = Env [] 0

push :: Frame -> Env -> Env
push f (Env fs i) = Env (f : fs) i

pop :: Env -> Env
pop (Env (_ : fs) i) = Env fs i
pop _ = error "pop: empty environment"

-- -- use State monad to generate fresh names
freshName :: State Int ResId
freshName = do
  n <- get
  put (n + 1)
  return n

define :: Text -> Env -> Env
define x (Env (Frame ns : fs) lastId) = do
  let (fresh, nextId) = runState freshName lastId
   in Env (Frame ((x, fresh) : ns) : fs) nextId
define x (Env [] lastId) =
  let (fresh, nextId) = runState freshName lastId
   in Env [Frame [(x, fresh)]] nextId

lookupVar :: Text -> Rename ResId
lookupVar n = do
  env <- get
  lookupVar' n env
  where
    lookupVar' :: Text -> Env -> Rename ResId
    lookupVar' n (Env [] id) = pure RenameError
    lookupVar' n (Env (Frame ns : fs) id) =
      case lookup n ns of
        Just resId -> pure $ Ok resId
        Nothing -> lookupVar' n (Env fs)

renameProgram :: Spanned A.Program -> Env -> Rename (Spanned Program)
renameProgram _ env = undefined

renameModule :: Spanned A.Module -> Env -> Rename (Spanned Module)
renameModule _ env = undefined

renameExpr :: Spanned A.Expr -> Rename (Spanned Expr)
renameExpr (Spanned (A.ELit lit) s) = pure $ Ok $ Spanned (ELit $ renameLit lit) s
renameExpr (Spanned (A.EVar (Spanned x _)) s) = do
  resId <- lookupVar x
  pure $ Ok $ Spanned (EVar (Spanned resId s)) s
renameExpr (Spanned (A.EApp e1 e2) s) = do
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  pure $ Ok $ Spanned (EApp e1' e2') s
renameExpr _ = undefined

renamePattern :: Spanned A.Pattern -> Env -> Spanned Pattern
renamePattern _ _ = undefined

renameLit :: Spanned A.Lit -> Spanned Lit
renameLit (Spanned (A.LInt i) s) = Spanned (LInt i) s
renameLit (Spanned (A.LBool b) s) = Spanned (LBool b) s
renameLit (Spanned (A.LString c) s) = Spanned (LString c) s