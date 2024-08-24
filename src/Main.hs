module Main where

import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Parser (replParse)
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShow)

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".tinyml_history"}

repl :: InputT IO ()
repl = do
  input <- getMultilineInput ""
  case input of
    Just i -> case replParse (pack i) of
      Left err -> outputStrLn $ "Error: " ++ unpack (pack (errorBundlePretty err))
      Right d -> outputStrLn $ unpack $ toStrict $ pShow d
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
