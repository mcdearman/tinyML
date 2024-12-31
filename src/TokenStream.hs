module TokenStream where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Text (Text, pack, unpack)
import Span (Span)
import Text.Megaparsec (PosState (..), SourcePos (sourceLine))
import Text.Megaparsec.Stream
import qualified Token as T
