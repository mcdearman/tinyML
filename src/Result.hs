module Result where

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