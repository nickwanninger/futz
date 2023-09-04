module Main where

-- import ErrM

-- import LexRelAlgebra(Token(..), Tok(..), BTree(..), resWords)
-- import ParRelAlgebra(myLexer, pRel)

import Data.List
import qualified Data.Set as Set
import qualified Futz.Lexer as L
import qualified Futz.Parser as P
import qualified Futz.Printer as PR
import qualified Futz.Syntax as S
import qualified Futz.TypeCheck as TC

import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Pretty.Simple (pPrint)


main = do
  args <- getArgs
  case args of
    [file] -> do
      handle <- openFile file ReadMode
      contents <- hGetContents handle
      putStrLn contents
      let lexRes = L.scanTokens contents
      case lexRes of
        Left err -> putStrLn err
        Right tokens -> do
          -- print tokens
          let ast = P.parseFutz tokens
          -- pPrint ast
          mapM_ typeCheckTest ast
    -- putStrLn $ PR.format ast
    _ -> putStrLn "Usage: futz <prog.futz>"

typeCheckTest decl@(S.Decl name body) = do
  putStrLn $ "\nTypechecking " <> show decl
  case TC.inferType body of
    Left err -> print err
    Right t -> do
      putStrLn $ "Typ: " <> show t

  return ()

-- putStrLn $ "Type: " <> (show $ TC.inferType body)
-- case TC.inferTop decl of
--   (t, constraints) -> do
--     putStrLn $ "Type: " <> TC.inferType t
--     -- putStrLn "Constraints:"
--     -- mapM_ print constraints
--     -- convert a list of constraints to a set of substitutions
--     -- which can be applied w/ `foldr apply ...`
--     let unification = TC.unify constraints
--     -- mapM_ print unification
--     case unification of
--       Left err -> return ()
--       Right unification -> putStrLn $ "Unified Type: " <> show (foldl (flip TC.apply) t unification)
--     return ()

typeCheckTest (S.TypeDecl _ _) = return ()
