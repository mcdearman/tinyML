module TokenStream where

import Data.Text (Text)
import Token

data TokenStream = TokenStream
  { src :: Text,
    tokens :: [Token]
  }
  deriving (Show)