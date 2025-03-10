module AST where

import Common (Spanned)
import Data.Array (Array)
import Data.Text (Text)

type Prog = Spanned Module

data Module = Module
  { moduleName :: !Name,
    moduleImports :: [Path],
    moduleDataDefs :: [DataDef],
    moduleTypeAliases :: [TypeAlias],
    moduleDefs :: Defs Pattern
  }

data Attr = Attr Expr deriving (Show, Eq)

data RecordDef = RecordDef
  { recordName :: Name,
    recordTyVars :: [TyVar],
    recordFields :: [(Name, Spanned TypeHint, Visibility)],
    recordVis :: Visibility
  }
  deriving (Show, Eq)

data DataDef = DataDef
  { dataName :: Name,
    dataTyVars :: [TyVar],
    dataConstructors :: [(Name, [Spanned TypeHint])],
    dataVis :: Visibility
  }
  deriving (Show, Eq)

data TypeAlias = TypeAlias
  { aliasName :: Name,
    aliasTyVars :: [TyVar],
    aliasTy :: Spanned TypeHint,
    aliasVis :: Visibility
  }
  deriving (Show, Eq)

type Defs a = [Def a]

data Def a = Def
  { defAlts :: [Alt a],
    defVis :: Visibility
  }
  deriving (Show, Eq)

data Alt a = Alt a (Spanned Expr) deriving (Show, Eq)

data Visibility = Public | Private deriving (Show, Eq)

type Expr = Spanned ExprKind

data ExprKind
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
