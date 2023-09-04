{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}


module Main where

-- import ErrM
import Text.Pretty.Simple (pPrint)


import qualified Futz.Lexer as L
import qualified Futz.Parser as P
import qualified Futz.Syntax as S
import qualified Futz.Printer as PR
import qualified Futz.TypeCheck as TC
-- import LexRelAlgebra(Token(..), Tok(..), BTree(..), resWords)
-- import ParRelAlgebra(myLexer, pRel)


import Data.List
import System.IO
import System.Process
import System.Environment
import System.Exit
import qualified Data.Set as Set


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
            print tokens
            let ast = P.parseFutz tokens
            -- pPrint ast
            mapM_ typeCheckTest ast
            -- putStrLn $ PR.format ast
    _ -> putStrLn "Usage: futz <prog.futz>"

  
typeCheckTest decl@(S.Decl name body) = do
  putStrLn $ "\n\nTypechecking '" <> name <> "':"
  print body
  case TC.inferTop decl of
    (t, constraints) -> do
      -- putStrLn $ "Type: " <> show t
      -- putStrLn "Constraints:"
      -- mapM_ print constraints
      -- convert a list of constraints to a set of substitutions
      -- which can be applied w/ `foldr apply ...`
      let unification = TC.unify constraints
      -- mapM_ print unification
      case unification of
        Left err -> return ()
        Right unification -> putStrLn $ "Unified Type: " <> show (foldl (flip TC.apply) t unification)
      -- case TC.unifyType t constraints of
        -- Left err -> return ()
        -- Right (unified, subst, cs) -> do
        --   putStrLn $ "Unified: " <> show unified
        --   mapM_ (putStrLn . show) subst
      return ()


typeCheckTest (S.TypeDecl _ _) = return ()
