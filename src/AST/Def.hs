module AST.Def where

import AST.Expr
import Data.Text
import Spanned

data Def = Def (Spanned Text) (Spanned Expr) deriving (Show)