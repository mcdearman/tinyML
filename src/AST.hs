module AST
  ( Prog,
    Module (..),
    Attr,
    RecordDef,
    RecordDefSort (..),
    DataDef,
    DataDefSort (..),
    TypeAlias,
    TypeAliasSort (..),
    Def,
    DefSort (..),
    FnDef,
    FnDefSort (..),
    Visibility (..),
    Expr,
    ExprSort (..),
    UnOp,
    UnOpSort (..),
    unOpName,
    BinOp,
    BinOpSort (..),
    binOpName,
    TypeAnno,
    TypeAnnoSort (..),
    Pattern,
    PatternSort (..),
    TyVar,
    Name,
    Path,
    Lit (..),
  )
where

import Common (Rational64, Spanned)
import Data.Array (Array)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word8)

type Prog = Spanned Module

data Module = Module
  { moduleName :: !Name,
    moduleImports :: [Path],
    moduleRecordDefs :: [RecordDef],
    moduleDataDefs :: [DataDef],
    moduleTypeAliases :: [TypeAlias],
    moduleDefs :: [Def],
    moduleFnDefs :: [FnDef]
  }

type Attr = Spanned Expr

type RecordDef = Spanned RecordDefSort

data RecordDefSort = RecordDef
  { recordName :: !Name,
    recordTyVars :: [TyVar],
    recordFields :: [(Name, Spanned TypeAnno, Visibility)],
    recordVis :: !Visibility
  }
  deriving (Show, Eq)

type DataDef = Spanned DataDefSort

data DataDefSort = DataDef
  { dataName :: !Name,
    dataTyVars :: [TyVar],
    dataConstructors :: [(Name, [Spanned TypeAnno])],
    dataVis :: !Visibility
  }
  deriving (Show, Eq)

type TypeAlias = Spanned TypeAliasSort

data TypeAliasSort = TypeAlias
  { aliasName :: !Name,
    aliasTyVars :: [TyVar],
    aliasTy :: Spanned TypeAnno,
    aliasVis :: !Visibility
  }
  deriving (Show, Eq)

type Def = Spanned DefSort

data DefSort = Def
  { defPat :: !Pattern,
    defTyAnno :: Maybe TypeAnno,
    defBody :: Spanned Expr,
    defVis :: !Visibility
  }
  deriving (Show, Eq)

type FnDef = Spanned FnDefSort

data FnDefSort = FnDef
  { fnName :: !Name,
    fnTyAnno :: Maybe TypeAnno,
    fnArgs :: [Spanned Pattern],
    fnBody :: Expr,
    fnVis :: !Visibility
  }
  deriving (Show, Eq)

data Visibility = Public | Private deriving (Show, Eq)

type Expr = Spanned ExprSort

data ExprSort
  = Lit !Lit
  | Var !Name
  | App Expr Expr
  | Lam [Pattern] Expr
  | Let Pattern Expr Expr
  | Fn !Name [Pattern] Expr Expr
  | Unary !UnOp Expr
  | Binary !BinOp Expr Expr
  | If Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  | List [Expr]
  | Array !(Array Word Expr)
  | Tuple [Expr]
  | Record !(Maybe Name) [(Name, Expr)]
  | Unit
  deriving (Show, Eq)

type UnOp = Spanned UnOpSort

data UnOpSort
  = UnOpNeg
  | UnOpNot
  deriving (Show, Eq)

unOpName :: UnOpSort -> Text
unOpName UnOpNeg = "neg"
unOpName UnOpNot = "not"

type BinOp = Spanned BinOpSort

data BinOpSort
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

binOpName :: BinOpSort -> Text
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

type TypeAnno = Spanned TypeAnnoSort

data TypeAnnoSort
  = TypeAnnoInt
  | TypeAnnoBool
  | TypeAnnoString
  | TypeAnnoVar TyVar
  | TypeAnnoIdent !Name
  | TypeAnnoKind !Name [TypeAnno]
  | TypeAnnoList TypeAnno
  | TypeAnnoArray TypeAnno
  | TypeAnnoTuple [TypeAnno]
  | TypeAnnoArrow TypeAnno TypeAnno
  | TypeAnnoRecord !(Maybe Name) [(Name, TypeAnno)]
  | TypeAnnoUnit
  deriving (Show, Eq)

type Pattern = Spanned PatternSort

data PatternSort
  = PatternWildcard
  | PatternLit !Lit
  | PatternVar !Name
  | PatternPair Pattern Pattern
  | PatternList [Pattern]
  | PatternUnit
  deriving (Show, Eq)

type TyVar = Spanned Text

type Name = Spanned Text

type Path = Spanned [Name]

data Lit
  = Byte !Word8
  | Int !Int64
  | Rational !Rational64
  | Real !Double
  | Bool !Bool
  | String !Text
  | Char !Char
  deriving (Show, Eq)
