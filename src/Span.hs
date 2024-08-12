module Span where


data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Show, Eq)
