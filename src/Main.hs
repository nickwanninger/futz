module Main where

-- import ErrM

-- import LexRelAlgebra(Token(..), Tok(..), BTree(..), resWords)
-- import ParRelAlgebra(myLexer, pRel)

import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Futz.Infer
import qualified Futz.Lexer as L
import qualified Futz.Module as Module
import qualified Futz.Parser as P
import Futz.Syntax
import Futz.Types
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Pretty.Simple (pPrint)

import qualified Data.Text as Text

main = do
  args <- getArgs
  case map Text.pack args of
    [file] -> do
      mod <- Module.load file
      return ()
    _ -> putStrLn "Usage: futz <prog.futz>"
