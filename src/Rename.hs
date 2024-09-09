module Rename where

import qualified AST as A
import Data.Text (Text)
import NIR
import Spanned

newtype Env = Env [Frame] deriving (Show, Eq)

newtype Frame = Frame [(Text, Name)] deriving (Show, Eq)

push :: Frame -> Env -> Env
push f (Env fs) = Env (f : fs)

pop :: Env -> Env
pop (Env (_ : fs)) = Env fs
pop _ = error "pop: empty environment"

define :: Text -> Name -> Env -> Env
define x n (Env (f : fs)) = Env (Frame ((x, n) : entries f) : fs)
define _ _ _ = error "define: empty environment"

entries :: Frame -> [(Text, Name)]
entries = undefined

renameProgram :: A.Program -> Program
renameProgram _ = undefined

renameModule :: A.Module -> Module
renameModule _ = undefined

renameExpr :: Spanned A.Expr -> Spanned Expr
renameExpr (Spanned (A.ELit lit) s) = Spanned (ELit $ renameLit lit) s

-- use freshName to generate a new name
-- renameExpr (Spanned (A.EVar name) s) =

renameLit :: Spanned A.Lit -> Spanned Lit
renameLit (Spanned (A.LInt i) s) = Spanned (LInt i) s
renameLit (Spanned (A.LBool b) s) = Spanned (LBool b) s
renameLit (Spanned (A.LString c) s) = Spanned (LString c) s