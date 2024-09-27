module Compiler where

import Data.Text (Text)
import Infer (Solver)
import Rename (Resolver)

data Compiler = Compiler
  { src :: Text,
    flags :: [Text],
    resolver :: Resolver,
    solver :: Solver
  }
  deriving (Show)