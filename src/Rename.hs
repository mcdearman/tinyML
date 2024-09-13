module Rename where

import qualified AST as A
import Control.Monad.State
import Data.Text (Text)
import NIR
import Span
import Spanned
import Unique (Unique (Id))

newtype RenameError = UnboundVariable Span deriving (Show, Eq)

data RenameResult a = Ok a | RenameError deriving (Show, Eq)

newtype Rename = Rename
  { counter :: State Int ResId
  }

newtype Env = Env [Frame] deriving (Show, Eq)

newtype Frame = Frame [(Text, ResId)] deriving (Show, Eq)

defaultEnv :: Env
defaultEnv = Env []

push :: Frame -> Env -> Env
push f (Env fs) = Env (f : fs)

pop :: Env -> Env
pop (Env (_ : fs)) = Env fs
pop _ = error "pop: empty environment"

-- -- use State monad to generate fresh names
freshName :: State Int ResId
freshName = do
  n <- get
  put (n + 1)
  return $ Id n

define :: Text -> Env -> Env
define x (Env (Frame ns : fs)) = do
  let (fresh, nextId) = runState freshName
   in Env (Frame ((x, fresh) : ns) : fs) nextId
define x (Env []) =
  let (fresh, nextId) = runState freshName lastId
   in Env [Frame [(x, fresh)]] nextId

lookupVar :: Text -> ResId
lookupVar n = do
  env <- get
  lookupVar' n env
  where
    lookupVar' :: Text -> Env -> ResId
    lookupVar' n (Env []) = pure RenameError
    lookupVar' n (Env (Frame ns : fs)) =
      case lookup n ns of
        Just resId -> pure $ Ok resId
        Nothing -> lookupVar' n (Env fs)

renameProgram :: Spanned A.Program -> Env -> Spanned Program
renameProgram _ env = undefined

renameModule :: Spanned A.Module -> Env -> Spanned Module
renameModule _ env = undefined

renameExpr :: Spanned A.Expr -> Env -> RenameResult (Spanned Expr, Env)
renameExpr (Spanned (A.ELit lit) s) env = Ok (Spanned (ELit $ renameLit lit) s, env)
-- renameExpr (Spanned (A.EVar (Spanned x _)) s) = do
--   resId <- lookupVar x
--   pure $ Ok $ Spanned (EVar (Spanned resId s)) s
-- renameExpr (Spanned (A.EApp e1 e2) s) = do
--   e1' <- renameExpr e1
--   e2' <- renameExpr e2
--   pure $ Ok $ Spanned (EApp e1' e2') s
renameExpr _ _ = undefined

renamePattern :: Spanned A.Pattern -> Env -> Spanned Pattern
renamePattern _ _ = undefined

renameLit :: Spanned A.Lit -> Spanned Lit
renameLit (Spanned (A.LInt i) s) = Spanned (LInt i) s
renameLit (Spanned (A.LBool b) s) = Spanned (LBool b) s
renameLit (Spanned (A.LString c) s) = Spanned (LString c) s