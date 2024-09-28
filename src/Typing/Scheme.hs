module Typing.Scheme where

import Typing.Ty

data Scheme = Scheme [TyVar] Ty deriving (Show)