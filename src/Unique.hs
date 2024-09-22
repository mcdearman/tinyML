module Unique where

newtype Unique = Id Int deriving (Show, Eq, Ord)

instance Semigroup Unique where
  Id i <> Id j = Id (i + j)