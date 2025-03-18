module Common where

import Data.Int (Int64)
import Data.Ratio (Ratio)
import Data.Text

class Pretty a where
  pretty :: a -> Text

instance (Pretty a) => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x : xs) = pretty x <> ",\n" <> pretty xs

data Result e v
  = Err e
  | Ok v
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap _ (Err e) = Err e
  fmap f (Ok v) = Ok (f v)

instance Applicative (Result e) where
  pure = Ok
  Err e <*> _ = Err e
  _ <*> Err e = Err e
  Ok f <*> Ok v = Ok (f v)

instance Monad (Result e) where
  Err e >>= _ = Err e
  Ok v >>= f = f v

data Span
  = SrcLoc {start :: Int, end :: Int}
  | Gen Span
  | NoLoc
  deriving (Show, Eq, Ord)

defaultSpan :: Span
defaultSpan = SrcLoc 0 0

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

data Spanned a = Spanned
  { spannedVal :: a,
    span :: Span
  }
  deriving (Show, Eq, Ord)

instance (Pretty a) => Pretty (Spanned a) where
  pretty (Spanned v s) = pretty v <> " @ " <> pretty s

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s

newtype Unique = Id Int deriving (Show, Eq, Ord)

type Rational64 = Ratio Int64