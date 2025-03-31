-- module TMC.Core where

-- import Common (Spanned, Unique)

-- data Program
--   = PFile Unique (Spanned Module)
--   | PRepl (Either (Typed Decl) (Typed Expr))

-- data Module = Module Unique [Typed Decl]

-- data Decl
--   = DDef (Typed Name) (Typed Expr)

-- data Expr
--   = ELit Lit
--   | EVar Name
--   | EApp (Typed Expr) (Typed Expr)
--   | ELam (Typed Pattern) (Typed Expr)
--   | ELet (Typed Pattern) (Typed Expr) (Typed Expr) Bool
--   | Join (Typed Expr) (Typed Expr)
--   | EMatch (Typed Expr) [(Typed Pattern, Typed Expr)]
--   | ECtorApp Name [Typed Expr]
--   | ERecord [(Name, Typed Expr)]
--   | ECast (Typed Expr) Ty

-- type Name = Spanned Unique

-- data Pattern
--   = PWildcard
--   | PLit (Spanned Lit)
--   | PVar Name
--   | PPair (Spanned Pattern) (Spanned Pattern)
--   | PList [Spanned Pattern]
--   | PUnit
--   deriving (Show, Eq)

-- data Lit
--   = LInt Int
--   | LBool Bool
--   | LChar Char
--   | LString String
--   | LUnit
--   deriving (Show, Eq)
