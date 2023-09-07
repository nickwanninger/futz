module Main where

-- import ErrM

-- import LexRelAlgebra(Token(..), Tok(..), BTree(..), resWords)
-- import ParRelAlgebra(myLexer, pRel)

import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Futz.Infer
import qualified Futz.Lexer as L
import qualified Futz.Parser as P
import qualified Futz.Printer as PR
import Futz.Syntax
import Futz.Types
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Pretty.Simple (pPrint)

-- temporary helper!
parseType :: String -> Type
parseType s = case L.scanTokens ("s :: " <> s) of
  Left err -> undefined
  Right tokens ->
    let ast = P.parseFutz tokens
     in case ast of
          [TypeDecl name t] -> t
          _ -> undefined

main = do
  args <- getArgs
  case args of
    [file] -> do
      handle <- openFile file ReadMode
      contents <- hGetContents handle
      -- putStrLn contents
      let lexRes = L.scanTokens contents
      case lexRes of
        Left err -> putStrLn err
        Right tokens -> do
          -- let t1 = parseType "'a0 -> 'a1"
          -- let t2 = parseType "('b -> 'c) -> ('a -> 'b) -> 'a -> 'c"
          -- print (quantify [tyvar "a0"] ([] :=> t1))
          -- return ()
          let ast = P.parseFutz tokens
          let prog = toProgram ast
          let env = exampleInsts initialEnv
          case env of
            Nothing -> print "fail!"
            Just env -> do
              let as = initialAssumptions ast
              putStrLn "\nInitial:"
              mapM_ print as
              -- putStrLn "\nInferring:"
              let as' = tiProgram env as prog
              putStrLn "\nNew Inferrence:"
              mapM_ print as'
    _ -> putStrLn "Usage: futz <prog.futz>"

dumbScheme :: Type -> Scheme
dumbScheme t = quantify (tv t) ([] :=> t)

initialAssumptions :: [TopLevel] -> [Assump]
initialAssumptions (v@(Decl name val) : tls) = a : initialAssumptions tls
  where
    a = name :>: dumbScheme (TVar (tyvar "a"))
initialAssumptions (v@(TypeDecl name t) : ts) = (name :>: dumbScheme t) : initialAssumptions ts
initialAssumptions [] = []

toProgram :: [TopLevel] -> Program
toProgram (v@(Decl name val) : tls) = dec : toProgram tls
  where
    im = Impl name [Binding [] val]
    dec = BindGroup [] [im]
toProgram (t : ts) = toProgram ts
toProgram [] = []
