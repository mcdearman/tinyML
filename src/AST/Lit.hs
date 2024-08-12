module AST.Lit where

data Lit
  = Int Integer
  | Bool Bool
  | String String
  deriving (Show)
