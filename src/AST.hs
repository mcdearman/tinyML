module AST where

import Common (Spanned)
import Data.Array (Array)
import Data.Text (Text)

type Prog = Spanned Module

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

data Visibility = Public | Private deriving (Show, Eq)

data Expr
  = Lit Lit
  | Var Name
  | App (Spanned Expr) (Spanned Expr)
  | Lam [Spanned Pattern] (Spanned Expr)
  | Let (Spanned Pattern) (Spanned Expr) (Spanned Expr)
  | Fn Name [Spanned Pattern] (Spanned Expr) (Spanned Expr)
  | Unary (Spanned UnOp) (Spanned Expr)
  | Binary (Spanned BinOp) (Spanned Expr) (Spanned Expr)
  | If (Spanned Expr) (Spanned Expr) (Spanned Expr)
  | Match (Spanned Expr) [(Spanned Pattern, Spanned Expr)]
  | List [Spanned Expr]
  | Array (Array Int (Spanned Expr))
  | Tuple [Spanned Expr]
  | Record (Maybe Name) [(Name, Spanned Expr)]
  | Unit
  deriving (Show, Eq)

data UnOp
  = UnOpNeg
  | UnOpNot
  deriving (Show, Eq)

unOpName :: UnOp -> Text
unOpName UnOpNeg = "neg"
unOpName UnOpNot = "not"

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpMod
  | BinOpPow
  | BinOpAnd
  | BinOpOr
  | BinOpEq
  | BinOpNeq
  | BinOpGt
  | BinOpLt
  | BinOpGeq
  | BinOpLeq
  | BinOpPair
  | BinOpPipe
  deriving (Show, Eq)

binOpName :: BinOp -> Text
binOpName BinOpAdd = "+"
binOpName BinOpSub = "-"
binOpName BinOpMul = "*"
binOpName BinOpDiv = "/"
binOpName BinOpMod = "%"
binOpName BinOpPow = "^"
binOpName BinOpAnd = "and"
binOpName BinOpOr = "or"
binOpName BinOpEq = "=="
binOpName BinOpNeq = "!="
binOpName BinOpGt = ">"
binOpName BinOpLt = "<"
binOpName BinOpGeq = ">="
binOpName BinOpLeq = "<="
binOpName BinOpPair = "::"
binOpName BinOpPipe = "|>"

data TypeHint
  = TypeHintInt
  | TypeHintBool
  | TypeHintString
  | TypeHintVar TyVar
  | TypeHintIdent Name
  | TypeHintKind Name [Spanned TypeHint]
  | TypeHintList (Spanned TypeHint)
  | TypeHintArray (Spanned TypeHint)
  | TypeHintTuple [Spanned TypeHint]
  | TypeHintArrow (Spanned TypeHint) (Spanned TypeHint)
  | TypeHintRecord (Maybe Name) [(Name, Spanned TypeHint)]
  | TypeHintUnit
  deriving (Show, Eq)

data Pattern
  = PatternWildcard
  | PatternLit Lit
  | PatternVar Name
  | PatternPair (Spanned Pattern) (Spanned Pattern)
  | PatternList [Spanned Pattern]
  | PatternUnit
  deriving (Show, Eq)

type TyVar = Spanned Text

type Name = Spanned Text

type Path = Spanned [Name]

data Lit
  = LitInt Int
  | LitBool Bool
  | LitString Text
  deriving (Show, Eq)
