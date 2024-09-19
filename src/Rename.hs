module Rename where

import qualified AST as A
import Control.Monad.State
import Data.Text (Text)
import NIR
import Result
import Span
import Spanned
import Unique (Unique (Id))
import Prelude hiding (span)

newtype RenameError = UnboundVariable Span deriving (Show, Eq)

newtype Env = Env [Frame] deriving (Show, Eq)

newtype Frame = Frame [(Text, ResId)] deriving (Show, Eq)

data Resolver = Resolver
  { resId :: ResId,
    env :: Env,
    errors :: [RenameError]
  }
  deriving (Show, Eq)

type ResState a = State Resolver a

defaultEnv :: Env
defaultEnv = Env []

push :: Frame -> Env -> Env
push f (Env fs) = Env (f : fs)

pop :: Env -> Env
pop (Env (_ : fs)) = Env fs
pop _ = error "pop: empty environment"

-- use State monad to generate fresh names
freshName :: ResState ResId
freshName = do
  r <- get
  let Id n = resId r
  put r {resId = Id $ n + 1}
  pure $ Id n

define :: Text -> Env -> ResState Env
define x (Env (Frame ns : fs)) = do
  resId <- freshName
  pure $ Env (Frame ((x, resId) : ns) : fs)
define x (Env []) = do
  resId <- freshName
  pure $ Env [Frame [(x, resId)]]

lookupVar :: Text -> Env -> Result RenameError ResId
lookupVar x (Env (Frame ns : fs)) = case lookup x ns of
  Just resId -> Ok resId
  Nothing -> lookupVar x (Env fs)
lookupVar _ (Env []) = Err $ UnboundVariable NoLoc

-- rename :: Spanned A.Program -> Res (Spanned Program)
-- rename p = do
--   let (p', _) = runState (renameProgram p defaultEnv) 0
--   pure p'

-- renameProgram :: Spanned A.Program -> Env -> Res (Spanned Program, Env)
-- renameProgram p env = case p of
--   Spanned (A.PFile n m) s -> do
--     (m', env') <- renameModule m env
--     pure $ Ok (Spanned (PFile n m') s, env')
--   Spanned (A.PRepl m) s -> do
--     let (m', env') = renameModule m env
--     pure $ Ok (Spanned (PRepl m') s, env')

-- renameModule :: Spanned A.Module -> Env -> Res (Spanned Module, Env)
-- renameModule m env = case m of
--   Spanned (A.Module n ds) s -> do
--     n' <- freshName
--     let (ds', env') = renameDecls ds env
--     pure (Spanned (Module (Spanned n' (span n)) ds') s, env')
--   where
--     renameDecls :: [Spanned A.Decl] -> Env -> ([Spanned Decl], Env)
--     renameDecls [] env = ([], env)
--     renameDecls (d : ds) env = let (d', env') = renameDecl d env in let (ds', env'') = renameDecls ds env' in (d' : ds', env'')

renameDecl :: Spanned A.Decl -> Env -> ResState (Spanned Decl, Env)
renameDecl d env = undefined

renameExpr :: Spanned A.Expr -> Env -> ResState (Spanned Expr, Env)
-- renameExpr (Spanned (A.ELit lit) s) env = pure $ Ok (Spanned (ELit (renameLit lit)) s, env)
-- renameExpr (Spanned (A.EVar (Spanned x _)) s) env =
--   case lookupVar x env of
--     Ok resId -> pure $ Ok (Spanned (EVar (Spanned resId s)) s, env)
--     Err e -> pure $ Err e
-- renameExpr (Spanned (A.EApp f arg) s) env = undefined
renameExpr _ _ = undefined

renamePattern :: Spanned A.Pattern -> Env -> Spanned Pattern
renamePattern _ _ = undefined

renameLit :: Spanned A.Lit -> Spanned Lit
renameLit (Spanned (A.LInt i) s) = Spanned (LInt i) s
renameLit (Spanned (A.LBool b) s) = Spanned (LBool b) s
renameLit (Spanned (A.LString c) s) = Spanned (LString c) s