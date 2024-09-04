module TokenStream where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Text (Text, pack, unpack)
import Text.Megaparsec (PosState (..), SourcePos (sourceLine))
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

  tokenToChunk _ = pure
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream src (t : ts)) = Just (t, TokenStream src ts)

  takeN_ n ts@(TokenStream src s)
    | n <= 0 = Just ([], ts)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream src s')
              Just nex -> Just (x, TokenStream (pack (drop (tokensLength pxy nex) (unpack src))) s')

  takeWhile_ f (TokenStream src ts) = (takeWhile f ts, TokenStream src (dropWhile f ts))

instance VisualStream TokenStream where
  showTokens _ = unwords . map (show . val) . NE.toList
  tokensLength _ ts = sum (NE.map len ts)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine),
      PosState
        { pstateInput =
            TokenStream
              { src = pack postStr,
                tokens = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case tokens pstateInput of
            [] -> pstateSourcePos
            xs -> end (last xs)
          (x : _) -> start x
      (pre, post) = splitAt (o - pstateOffset) (tokens pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (unpack $ src pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy