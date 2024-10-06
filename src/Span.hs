module Span where

import Data.Text (pack)
import Pretty

data Span
  = SrcLoc {start :: Int, end :: Int}
  | Gen Span
  | NoLoc
  deriving (Show, Eq, Ord)

instance Pretty Span where
  pretty (SrcLoc s e) = pack $ show s <> ".." <> show e
  pretty (Gen s) = "Gen " <> pretty s
  pretty NoLoc = "NoLoc"

instance Semigroup Span where
  SrcLoc s1 e1 <> SrcLoc s2 e2 = SrcLoc (min s1 s2) (max e1 e2)
  Gen s1 <> Gen s2 = Gen (s1 <> s2)
  Gen s <> s' = Gen (s <> s')
  s <> Gen s' = Gen (s <> s')
  _ <> _ = NoLoc