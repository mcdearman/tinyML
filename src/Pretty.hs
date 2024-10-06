module Pretty where

import Data.Text

class Pretty a where
  pretty :: a -> Text

instance (Pretty a) => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x : xs) = pretty x <> ",\n" <> pretty xs