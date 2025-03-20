module Lexer (TokenStream (..), WithPos (..), tokenize) where

import Common (Span (..), defaultSpan)
import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators (manyTill_)
import Data.Data (Proxy (Proxy))
import Data.Functor (($>))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    PosState (..),
    SourcePos (sourceLine),
    between,
    choice,
    getOffset,
    getSourcePos,
    many,
    manyTill,
    parse,
    sepEndBy,
    sepEndBy1,
    some,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    char',
    lowerChar,
    space1,
    string,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Stream hiding (Token)
import qualified Text.Megaparsec.Stream as S
import Token
import Prelude hiding (span)

data TokenStream = TokenStream
  { streamSrc :: Text,
    streamTokens :: [WithPos Token],
    streamLastSpan :: Span
  }
  deriving (Show, Eq, Ord)

data WithPos a = WithPos
  { wpStart :: SourcePos,
    wpEnd :: SourcePos,
    wpSpan :: Span,
    wpLen :: Int,
    wpVal :: a
  }
  deriving (Show, Eq, Ord)

instance Stream TokenStream where
  type Token TokenStream = WithPos Token
  type Tokens TokenStream = [WithPos Token]

  tokenToChunk _ = pure
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokenStream _ [] _) = Nothing
  take1_ (TokenStream src (t : ts) _) = case wpVal t of
    TokComment -> take1_ (TokenStream src ts (wpSpan t))
    _ -> Just (t, TokenStream src ts (wpSpan t))

  takeN_ n ts@(TokenStream src s ls)
    | n <= 0 = Just ([], ts)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream src s' ls)
              Just nex ->
                Just
                  (x, TokenStream (pack (drop (tokensLength pxy nex) (unpack src))) s' (wpSpan (last x)))

  takeWhile_ f (TokenStream src ts _) =
    let ts' = takeWhile f ts
     in (ts', TokenStream src (dropWhile f ts) (wpSpan (last ts')))

instance VisualStream TokenStream where
  showTokens _ = unwords . map (show . wpVal) . NE.toList
  tokensLength _ ts = sum (NE.map wpLen ts)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine),
      PosState
        { pstateInput =
            TokenStream
              { streamSrc = pack postStr,
                streamTokens = post,
                streamLastSpan = wpSpan (last post)
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
          [] -> case streamTokens pstateInput of
            [] -> pstateSourcePos
            xs -> wpEnd (last xs)
          (x : _) -> wpStart x
      (pre, post) = splitAt (o - pstateOffset) (streamTokens pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (unpack $ streamSrc pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy

type Lexer = Parsec Void Text

withPos :: Lexer a -> Lexer (WithPos a)
withPos p = do
  startPos <- getSourcePos
  startOffset <- getOffset
  result <- p
  endPos <- getSourcePos
  endOffset <- getOffset
  return $ WithPos startPos endPos (SrcLoc startOffset endOffset) (endOffset - startOffset) result

lexemeWithPos :: Lexer a -> Lexer (WithPos a)
lexemeWithPos p = withPos p <* sc

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "--") empty

octal :: Lexer Int64
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Lexer Int64
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Lexer Token
int = TokInt <$> (try octal <|> try hexadecimal <|> L.decimal)

real :: Lexer Token
real = TokReal <$> L.float

bool :: Lexer Token
bool = TokBool <$> choice [True <$ string "true", False <$ string "false"]

str :: Lexer Token
str = TokString <$> (char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')))

ident :: Lexer Token
ident = try $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar)
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be an identifier"
    else return $ TokLowerCaseIdent name
  where
    identStartChar = lowerChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

    keywords :: [Text]
    keywords =
      [ "module",
        "import",
        "as",
        "pub",
        "def",
        "let",
        "in",
        "if",
        "then",
        "else",
        "match",
        "with",
        "true",
        "false",
        "data",
        "type",
        "class",
        "instance",
        "derive",
        "do",
        "end"
      ]

upperCaseIdent :: Lexer Token
upperCaseIdent = TokUpperCaseIdent . pack <$> ((:) <$> upperChar <*> many alphaNumChar)

token :: Lexer (WithPos Token)
token =
  lexemeWithPos $
    choice
      [ TokComment <$ string "--" <* manyTill_ L.charLiteral (char '\n'),
        try real <|> int,
        bool,
        str,
        upperCaseIdent,
        ident,
        TokLParen <$ char '(',
        TokRParen <$ char ')',
        TokLBrace <$ char '{',
        TokRBrace <$ char '}',
        TokLBracket <$ char '[',
        TokRBracket <$ char ']',
        TokHash <$ char '#',
        TokPlus <$ char '+',
        TokMinus <$ char '-' <* notFollowedBy (char '>'),
        TokArrow <$ string "->",
        TokStar <$ char '*',
        TokSlash <$ char '/',
        TokBackSlash <$ char '\\',
        TokPercent <$ char '%',
        TokAnd <$ string "&&",
        TokOr <$ string "||",
        try (TokNeq <$ string "!=") <|> TokBang <$ char '!',
        try (TokFatArrow <$ string "=>") <|> TokEq <$ string "==" <|> TokAssign <$ char '=',
        TokLt <$ char '<',
        TokGt <$ char '>',
        TokLeq <$ string "<=",
        TokGeq <$ string ">=",
        try (TokDoublePeriod <$ string "..") <|> TokPeriod <$ char '.',
        TokComma <$ char ',',
        try (TokDoubleColon <$ string "::") <|> TokColon <$ char ':',
        TokSemiColon <$ char ';',
        try (TokPipe <$ string "|>") <|> TokBar <$ char '|',
        TokUnderscore <$ char '_',
        TokModule <$ string "module",
        TokImport <$ string "import",
        TokAs <$ string "as",
        TokPub <$ string "pub",
        TokDef <$ string "def",
        TokLet <$ string "let",
        TokIn <$ string "in",
        TokIf <$ string "if",
        TokThen <$ string "then",
        TokElse <$ string "else",
        TokMatch <$ string "match",
        TokWith <$ string "with",
        TokData <$ string "data",
        TokType <$ string "type",
        TokClass <$ string "class",
        TokInstance <$ string "instance",
        TokDerive <$ string "derive",
        TokDo <$ string "do",
        TokEnd <$ string "end"
      ]

tokenize :: Text -> Either (ParseErrorBundle Text Void) TokenStream
tokenize src = TokenStream src <$> (parse (many token) "" src)