module Spanned where

import Pretty
import Span

data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq, Ord)

instance (Pretty a) => Pretty (Spanned a) where
  pretty (Spanned v s) = pretty v <> " @ " <> pretty s

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s