module Span where

data Span
  = SrcLoc {start :: Int, end :: Int}
  | Gen Span
  | NoLoc
  deriving (Show, Eq)

instance Semigroup Span where
  SrcLoc s1 e1 <> SrcLoc s2 e2 = SrcLoc (min s1 s2) (max e1 e2)
  Gen s1 <> Gen s2 = Gen (s1 <> s2)
  Gen s <> s' = Gen (s <> s')
  s <> Gen s' = Gen (s <> s')
  _ <> _ = NoLoc