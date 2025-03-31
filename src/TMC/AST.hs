module TMC.AST
  ( Prog,
    Module (..),
    Def,
    DefSort (..),
    FnDef,
    FnDefSort (..),
    Expr,
    ExprSort (..),
    UnaryOp,
    UnOpSort (..),
    unOpName,
    BinaryOp,
    BinOpSort (..),
    binOpName,
    Pattern,
    PatternSort (..),
    Ident,
    Lit (..),
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import TMC.Common (Spanned)

type Prog = Spanned Module

data Module = Module
  { moduleName :: !Ident,
    moduleDefs :: [Def],
    moduleFns :: [FnDef]
  }

type Def = Spanned DefSort

data DefSort = Def {defPat :: !Pattern, defBody :: Expr} deriving (Show, Eq)

type FnDef = Spanned FnDefSort

data FnDefSort = FnDef
  { fnName :: !Ident,
    fnArgs :: [Pattern],
    fnBody :: Expr
  }
  deriving (Show, Eq)

type Expr = Spanned ExprSort

data ExprSort
  = Lit !Lit
  | Var !Ident
  | App Expr Expr
  | Lam [Pattern] Expr
  | Let Pattern Expr Expr
  | Fn !Ident [Pattern] Expr Expr
  | Unary !UnaryOp Expr
  | Binary !BinaryOp Expr Expr
  | If Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  | List [Expr]
  | Unit
  | Error
  deriving (Show, Eq)

type UnaryOp = Spanned UnOpSort

data UnOpSort
  = UnOpNeg
  deriving (Show, Eq)

unOpName :: UnOpSort -> Text
unOpName UnOpNeg = "prim__neg"

type BinaryOp = Spanned BinOpSort

data BinOpSort
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpMod
  | BinOpEq
  deriving (Show, Eq)

binOpName :: BinOpSort -> Text
binOpName BinOpAdd = "prim__add"
binOpName BinOpSub = "prim__sub"
binOpName BinOpMul = "prim__mul"
binOpName BinOpDiv = "prim__div"
binOpName BinOpMod = "prim__mod"
binOpName BinOpEq = "prim__eq"

type Pattern = Spanned PatternSort

data PatternSort
  = PatternWildcard
  | PatternLit !Lit
  | PatternVar !Ident
  | PatternUnit
  deriving (Show, Eq)

type Ident = Spanned Text

data Lit
  = Int !Int64
  | Bool !Bool
  | String !Text
  deriving (Show, Eq)
