module NIR where

import Data.Array (Array)
import Data.Text (Text)
import Spanned
import Unique (Unique)

data Program
  = PFile Text (Spanned Module)
  deriving (Show)

data Module = Module Name [Spanned Decl] deriving (Show)

data Decl
  = DDef (Spanned Pattern) (Spanned Expr)
  | DFn Name [Spanned Pattern] (Spanned Expr)
  | DFnMatch Name (Maybe (Spanned TypeHint)) [([Spanned Pattern], Spanned Expr)]
  | DRecordDef Name [TyVar] [(Name, Spanned TypeHint)]
  | DData Name [TyVar] [(Name, [Spanned TypeHint])]
  | DTypeSyn Name [TyVar] (Spanned TypeHint)
  | DImport Path
  deriving (Show, Eq)

data Expr
  = ELit Lit
  | EVar Name
  | EApp (Spanned Expr) (Spanned Expr)
  | ELam (Spanned Pattern) (Spanned Expr)
  | ELet (Spanned Pattern) (Spanned Expr) (Spanned Expr)
  | EFn Name [Spanned Pattern] (Spanned Expr) (Spanned Expr)
  | EIf (Spanned Expr) (Spanned Expr) (Spanned Expr)
  | EMatch (Spanned Expr) [(Spanned Pattern, Spanned Expr)]
  | EList [Spanned Expr]
  | EArray (Array Int (Spanned Expr))
  | ETuple [Spanned Expr]
  | ERecord (Maybe Name) [(Name, Spanned Expr)]
  | EUnit
  deriving (Show, Eq)

data TypeHint
  = THInt
  | THBool
  | THString
  | THVar TyVar
  | THIdent Name
  | THKind Name [Spanned TypeHint]
  | THList (Spanned TypeHint)
  | THArray (Spanned TypeHint)
  | THTuple [Spanned TypeHint]
  | THArrow (Spanned TypeHint) (Spanned TypeHint)
  | THRecord (Maybe Name) [(Name, Spanned TypeHint)]
  | THUnit
  deriving (Show, Eq)

data Pattern
  = PWildcard
  | PLit Lit
  | PVar Name
  | PPair (Spanned Pattern) (Spanned Pattern)
  | PList [Spanned Pattern]
  | PUnit
  deriving (Show, Eq)

type TyVar = Spanned Text

type Name = Spanned (Text, ResId)

type ResId = Unique

type Path = Spanned [Name]

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq)
