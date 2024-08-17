module Span where

data Span
  = SrcLoc {start :: Int, end :: Int}
  | Gen Span
  | NoLoc
  deriving (Show, Eq)
