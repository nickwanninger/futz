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
  case map Text.pack args of
    [file] -> do
      mod <- Module.load file
      pPrint mod
    -- handle <- openFile file ReadMode
    -- contents <- hGetContents handle
    -- -- putStrLn contents
    -- let lexRes = L.scanTokens contents
    -- case lexRes of
    --   Left err -> putStrLn err
    --   Right tokens -> do
    --     -- mapM_ print tokens
    --     let ast = P.parseFutz tokens
    --     mapM_ print ast
    --     let prog = fuseProgram ast
    --     let env = exampleInsts initialEnv
    --     case env of
    --       Nothing -> print "fail!"
    --       Just env -> do
    --         let as = initialAssumptions ast
    --         putStrLn "\nInitial:"
    --         print prog
    --         -- putStrLn "\nInferring:"
    --         let as' = tiProgram env [] [prog]
    --         putStrLn "\nNew Inferrence:"
    --         mapM_ print as'
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
