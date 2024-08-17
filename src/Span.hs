module Span where

-- data Span = Span
--   { start :: Int,
--     end :: Int
--   }
--   deriving (Show, Eq)

data Span
  = SrcLoc {start :: Int, end :: Int}
  | Gen Span
  | NoLoc
  deriving (Show, Eq)
