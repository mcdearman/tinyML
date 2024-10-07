module Compiler where

import Control.Monad.State.Strict
import Control.Placeholder (todo)
import Data.Function ((&))
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Lexer (lexMML)
import qualified NIR as NIR
import Parser (parseStream)
import Rename
import qualified Rename as Resolver
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShow)
import Typing.Infer
import Typing.Solver
import qualified Typing.Solver as Solver
import Unique

data Compiler = Compiler
  { src :: Text,
    flags :: [Text],
    resolver :: Resolver,
    solver :: Solver
  }
  deriving (Show)

defaultCompiler :: Compiler
defaultCompiler =
  Compiler
    { src = "",
      flags = [],
      resolver = defaultResolver,
      solver = defaultSolver
    }

type CompilerState a = State Compiler a

run :: Text -> CompilerState Text
run src = do
  Compiler {src = _, flags = f, resolver = r, solver = sl} <- get
  put $ Compiler {src = src, flags = f, resolver = r, solver = sl}
  case lexMML src of
    Left err -> pure $ pack $ "Lexer error: " ++ errorBundlePretty err
    Right d -> case parseStream d of
      Left err -> pure $ pack $ "Parser error: " ++ errorBundlePretty err
      Right p -> do
        let (nir, r') = runState (renameProgram p) r
        let r'' = r' & \res -> res {Resolver.errors = []}
        case r' of
          Resolver {errors = []} -> do
            put $ Compiler {src = src, flags = f, resolver = r', solver = sl}
            let (tir, sl') = runState (infer nir) sl
            let sl'' = sl' & \s -> s {Solver.errors = []}
            case sl' of
              Solver {errors = []} -> do
                put $ Compiler {src = src, flags = f, resolver = r', solver = sl'}
                pure $ (toStrict . pShow) tir
              Solver {errors = es} -> do
                put $ Compiler {src = src, flags = f, resolver = r', solver = sl''}
                pure $ pack $ "Type errors: " ++ show es
          Resolver {errors = es} -> do
            put $ Compiler {src = src, flags = f, resolver = r'', solver = sl}
            pure $ pack $ "Resolver errors: " ++ show es
