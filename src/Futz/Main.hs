{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}


module Futz.Main where

-- import ErrM
import Text.Pretty.Simple (pPrint)


import qualified Futz.Lexer as L
import qualified Futz.Parser as P
import qualified Futz.Syntax as S
import qualified Futz.Printer as PR
-- import LexRelAlgebra(Token(..), Tok(..), BTree(..), resWords)
-- import ParRelAlgebra(myLexer, pRel)


import Data.List
import System.IO
import System.Process
import System.Environment
import System.Exit


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

  
typeCheckTest (S.Decl name body) = do
  putStrLn $ "Typechecking '" <> name <> "':"
  pPrint body

typeCheckTest (S.TypeDecl _ _) = return ()
