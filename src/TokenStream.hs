module TokenStream where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Text (Text, pack)
import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Stream
import qualified Token as T

data TokenStream = TokenStream
  { src :: Text,
    tokens :: [WithPos T.Token]
  }
  deriving (Show, Eq, Ord)

data WithPos a = WithPos
  { start :: SourcePos,
    end :: SourcePos,
    len :: Int,
    val :: a
  }
  deriving (Show, Eq, Ord)

instance Stream TokenStream where
  type Token TokenStream = WithPos T.Token
  type Tokens TokenStream = [WithPos T.Token]

  tokenToChunk Proxy x = [x]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream src (t : ts)) = Just (t, TokenStream src ts)

  takeN_ n (TokenStream str s)
    | n <= 0 = Just ([], TokenStream str s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream str s')
              Just nex -> Just (x, TokenStream (pack (drop (tokensLength pxy nex) str) s'))

  takeWhile_ f (TokenStream src ts) = (takeWhile f ts, TokenStream src (dropWhile f ts))