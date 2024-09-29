module Typing.TIR where

import Control.Placeholder (todo)
import Data.Array
import Data.Text
import Spanned
import Typing.Ty (applySubst)
import qualified Typing.Ty as Ty
import Typing.Types
import Unique

data Program
  = PFile Text (Spanned Module)
  | PRepl (Either (Typed Decl) (Typed Expr))
  deriving (Show)

applySubstProgram :: Subst -> Spanned Program -> Spanned Program
applySubstProgram s (Spanned (PFile n m) sp) = todo
applySubstProgram s (Spanned (PRepl (Left d)) sp) =
  Spanned (PRepl (Left (applySubstDecl s d))) sp
applySubstProgram s (Spanned (PRepl (Right e)) sp) =
  Spanned (PRepl (Right (applySubstExpr s e))) sp

data Module = Module Name [Typed Decl] deriving (Show)

data Decl
  = DDef (Typed Pattern) (Typed Expr)
  | DFn Name [Typed Pattern] (Typed Expr)
  | DFnMatch Name [([Typed Pattern], Typed Expr)]
  | DRecordDef Name [TyVar] [(Name, Spanned Ty)]
  | DData Name [TyVar] [(Name, [Spanned Ty])]
  | DTypeSyn Name [TyVar] (Spanned Ty)
  | DImport Path
  deriving (Show, Eq)

applySubstDecl :: Subst -> Typed Decl -> Typed Decl
applySubstDecl s (Typed (Spanned (DDef p e) sp) t) =
  Typed (Spanned (DDef (applySubstPattern s p) (applySubstExpr s e)) sp) (Ty.applySubst s t)
applySubstDecl s (Typed (Spanned (DFn n ps e) sp) t) =
  Typed (Spanned (DFn n (fmap (applySubstPattern s) ps) (applySubstExpr s e)) sp) (Ty.applySubst s t)
applySubstDecl s (Typed (Spanned (DFnMatch n pes) sp) t) =
  Typed (Spanned (DFnMatch n (fmap (\(ps, e) -> (fmap (applySubstPattern s) ps, applySubstExpr s e)) pes)) sp) (Ty.applySubst s t)
applySubstDecl s (Typed (Spanned (DRecordDef n vs fs) sp) t) =
  Typed (Spanned (DRecordDef n vs (fmap (\(n, t) -> (n, fmap (Ty.applySubst s) t)) fs)) sp) (Ty.applySubst s t)
applySubstDecl _ _ = todo

data Expr
  = ELit Lit
  | EVar Name
  | EApp (Typed Expr) (Typed Expr)
  | ELam (Typed Pattern) (Typed Expr)
  | ELet (Typed Pattern) (Typed Expr) (Typed Expr)
  | EFn Name [Typed Pattern] (Typed Expr) (Typed Expr)
  | EIf (Typed Expr) (Typed Expr) (Typed Expr)
  | EMatch (Typed Expr) [(Typed Pattern, Typed Expr)]
  | EList [Typed Expr]
  | EArray (Array Int (Typed Expr))
  | ETuple [Typed Expr]
  | ERecord (Maybe Name) [(Name, Typed Expr)]
  | EUnit
  deriving (Show, Eq)

applySubstExpr :: Subst -> Typed Expr -> Typed Expr
applySubstExpr s (Typed e@(Spanned (ELit _) _) t) = Typed e (Ty.applySubst s t)
applySubstExpr s (Typed e@(Spanned (EVar _) _) t) = Typed e (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EApp e1 e2) sp) t) =
  Typed (Spanned (EApp (applySubstExpr s e1) (applySubstExpr s e2)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ELam p e) sp) t) =
  Typed (Spanned (ELam (applySubstPattern s p) (applySubstExpr s e)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ELet p e1 e2) sp) t) =
  Typed (Spanned (ELet (applySubstPattern s p) (applySubstExpr s e1) (applySubstExpr s e2)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EFn n ps e1 e2) sp) t) =
  Typed (Spanned (EFn n (fmap (applySubstPattern s) ps) (applySubstExpr s e1) (applySubstExpr s e2)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EIf e1 e2 e3) sp) t) =
  Typed (Spanned (EIf (applySubstExpr s e1) (applySubstExpr s e2) (applySubstExpr s e3)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EMatch e ps) sp) t) =
  Typed (Spanned (EMatch (applySubstExpr s e) (fmap (\(p, e) -> (applySubstPattern s p, applySubstExpr s e)) ps)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EList es) sp) t) =
  Typed (Spanned (EList (fmap (applySubstExpr s) es)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (EArray es) sp) t) =
  Typed (Spanned (EArray (fmap (applySubstExpr s) es)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ETuple es) sp) t) =
  Typed (Spanned (ETuple (fmap (applySubstExpr s) es)) sp) (Ty.applySubst s t)
applySubstExpr s (Typed (Spanned (ERecord n fs) sp) t) =
  Typed (Spanned (ERecord n (fmap (\(n, e) -> (n, applySubstExpr s e)) fs)) sp) (Ty.applySubst s t)
applySubstExpr _ e = e

data Pattern
  = PWildcard
  | PLit Lit
  | PVar Name
  | PPair (Typed Pattern) (Typed Pattern)
  | PList [Typed Pattern]
  | PUnit
  deriving (Show, Eq)

applySubstPattern :: Subst -> Typed Pattern -> Typed Pattern
applySubstPattern s (Typed p@(Spanned PWildcard _) t) = Typed p (Ty.applySubst s t)
applySubstPattern s (Typed p@(Spanned (PVar _) _) t) = Typed p (Ty.applySubst s t)
applySubstPattern s (Typed (Spanned (PPair p1 p2) sp) t) =
  Typed (Spanned (PPair (applySubstPattern s p1) (applySubstPattern s p2)) sp) (Ty.applySubst s t)
applySubstPattern s (Typed (Spanned (PList ps) sp) t) =
  Typed (Spanned (PList (fmap (applySubstPattern s) ps)) sp) (Ty.applySubst s t)
applySubstPattern _ p = p

type Name = Spanned (Text, ResId)

type ResId = Unique

type Path = Spanned [Name]

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
