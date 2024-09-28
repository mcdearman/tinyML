module Compiler where

import Data.Text (Text)
import Rename (Resolver)

data Compiler = Compiler
  { src :: Text,
    flags :: [Text],
    resolver :: Resolver
  }
  deriving (Show)