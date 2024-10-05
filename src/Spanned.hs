module Spanned where

import Span

data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq, Ord)

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s