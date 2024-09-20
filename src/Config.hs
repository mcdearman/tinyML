module Config where

import Data.Text (Text)

data Config = Config
  { debug :: Bool,
    interactive :: Bool,
    builtins :: [Text]
  }
  deriving (Show, Eq)