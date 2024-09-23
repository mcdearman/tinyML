module Main where

import Control.Monad.State (runState, evalState)
import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Lexer (lexMML)
import Parser
import Rename
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShow)
import Unique

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".tinyml_history"}

repl :: Resolver -> InputT IO ()
repl r = do
  input <- getMultilineInput ""
  case input of
    Just "env" -> do
      outputStrLn $ unpack $ (toStrict . pShow) (env r)
      repl r
    Just i -> case lexMML (pack i) of
      Left err -> outputStrLn $ "Lexer error: " ++ unpack (pack (errorBundlePretty err))
      Right d -> case parseStream d of
        Left err -> outputStrLn $ "Parser error: " ++ unpack (pack (errorBundlePretty err))
        Right p -> do
          outputStrLn $ unpack $ (toStrict . pShow) p
          let (nir, res) = runState (renameProgram p) r
          case res of
            Resolver {errors = []} -> do
              outputStrLn $ unpack $ (toStrict . pShow) nir
              repl res
            Resolver {errors = e} -> do
              outputStrLn $ "Resolver errors: " ++ show e
              repl r
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
  -- let emptyRes = Resolver {resId = Id 0, env = Env [], errors = []}
  let defaultResolver = evalState _ defaultEnv
  runInputT settings (repl defaultResolver)
