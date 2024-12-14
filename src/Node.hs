module Node where

data Node v m = Node
  { value :: v,
    meta :: m
  }
  deriving (Eq, Show)