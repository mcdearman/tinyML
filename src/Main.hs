module Main where

import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Lexer (lexMML)
import Parser
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShow)

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".tinyml_history"}

repl :: InputT IO ()
repl = do
  input <- getMultilineInput ""
  case input of
    Just i -> case lexMML (pack i) of
      Left err -> outputStrLn $ "Lexer error: " ++ unpack (pack (errorBundlePretty err))
      Right d -> case parseStream d of
        Left err -> outputStrLn $ "Parser error: " ++ unpack (pack (errorBundlePretty err))
        Right p -> outputStrLn $ unpack $ (toStrict . pShow) p
    Nothing -> return ()
  repl

getMultilineInput :: String -> InputT IO (Maybe String)
getMultilineInput acc = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> return Nothing
    Just fl -> collectLines (acc ++ fl ++ "\n")

collectLines :: String -> InputT IO (Maybe String)
collectLines acc = do
  minput <- getInputLine ""
  case minput of
    Nothing -> return Nothing
    Just "" -> return $ Just (init acc)
    Just input -> collectLines (acc ++ input ++ "\n")

main :: IO ()
main = do
  putStrLn "Welcome to the MiniML REPL!"
  runInputT settings repl
