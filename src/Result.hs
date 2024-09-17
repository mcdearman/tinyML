module Result where

data Result a b
  = Ok a
  | Err b
  deriving (Eq, Show)

instance Functor (Result a) where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Err b) = Err b

instance Applicative (Result a) where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Err b <*> _ = Err b
  _ <*> Err b = Err b

instance Monad (Result a) where
  Ok a >>= f = f a
  Err b >>= _ = Err b