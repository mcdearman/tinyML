module TIR where

import Data.Array
import Data.Text
import Spanned
import Ty
import Unique

data Program
  = PFile Text (Spanned Module)
  | PRepl (Either (Typed Decl) (Typed Expr))
  deriving (Show)

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
data Expr
  = ELit Lit
  | EVar Name
  | EApp (Typed Expr) (Typed Expr)
  | ELam (Typed Expr) (Typed Expr)
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

data Pattern
  = PWildcard
  | PLit Lit
  | PVar Name
  | PPair (Typed Pattern) (Typed Pattern)
  | PList [Typed Pattern]
  | PUnit
  deriving (Show, Eq)

type Name = Spanned (Text, ResId)

type ResId = Unique

type Path = Spanned [Name]

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
