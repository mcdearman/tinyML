module Common where

import Data.Text

-- data Node v m = Node
--   { value :: v,
--     meta :: m
--   }
--   deriving (Eq, Show)

class Pretty a where
  pretty :: a -> Text

instance (Pretty a) => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x : xs) = pretty x <> ",\n" <> pretty xs

data Result e v
  = Ok v
  | Err e
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap f (Ok v) = Ok (f v)
  fmap _ (Err e) = Err e

instance Applicative (Result e) where
  pure = Ok
  Ok f <*> Ok v = Ok (f v)
  Err e <*> _ = Err e
  _ <*> Err e = Err e

instance Monad (Result e) where
  Ok v >>= f = f v
  Err e >>= _ = Err e

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

data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq, Ord)

instance (Pretty a) => Pretty (Spanned a) where
  pretty (Spanned v s) = pretty v <> " @ " <> pretty s

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s

newtype Unique = Id Int deriving (Show, Eq, Ord)