module Main where

import Compiler (Compiler)
import qualified Compiler as Compiler
import Control.Monad.State (evalState, runState)
import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Lexer (lexMML)
import Parser
import Rename
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShow)

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".tinyml_history"}

repl :: Compiler -> InputT IO ()
repl c = do
  input <- getMultilineInput ""
  let r = Compiler.resolver c
  case input of
    Just "env" -> do
      outputStrLn $ unpack $ (toStrict . pShow) r
      repl c
    Just "res" -> do
      outputStrLn $ unpack $ (toStrict . pShow) ()
      repl c
    Just src -> do
      let (out, c') = runState (Compiler.run (pack src)) c
      outputStrLn $ unpack out
      repl c'
    Nothing -> return ()

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
  runInputT settings (repl Compiler.defaultCompiler)
