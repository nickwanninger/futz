module Futz.Module where

import Data.Either.Combinators
import Data.Text
import qualified Futz.Infer as Infer
import qualified Futz.Lexer as Lexer
import qualified Futz.Parser as Parser
import Futz.Syntax
import Futz.Types
import System.IO

-- This is a representation of a logical `Module`, which
-- is effectively the internal data format of a parsed file.
data Module = Module
  { -- The name of the module
    name :: Text,
    -- The raw AST, in case it's useful to have it around
    ast :: [TopLevel SourceRange],
    -- Bindings
    program :: Program SourceRange
  }
  deriving (Eq, Show)

-- Given a module name or path, attempt to load a module by parsing it
load :: Text -> IO (Maybe Module)
load name = do
  -- For now, consider the name to be a file path we can open. In the future
  -- we will have a module name resolution system, but that's down the line :)
  handle <- openFile (unpack name) ReadMode
  -- TODO: abstract loading and filenames
  contents <- hGetContents handle
  -- Lex the contents of the file
  case Lexer.scanTokens contents of
    -- If the lexer gives us an error, just print it and return Nothing. We
    -- need to make this load function return some kind of error, but I haven't
    -- decided on that representation yet.
    Left err -> do
      putStrLn err
      return Nothing
    -- If the tokenization works, parse the AST
    Right tokens -> do
      -- mapM_ print tokens
      let ast = Parser.parseFutz tokens
      mapM_ print ast
      let program = fuseProgram ast
      -- putStrLn "Program:"
      -- print program
      let env = exampleInsts initialEnv
      case env of
        Left err -> do
          putStrLn "Fatal error. Could not setup initial environment!"
          return Nothing
        Right env -> do
          -- print env
          let as = Prelude.map initialAssumption (defs program)
          -- mapM_ print as
          case Infer.tiProgram env as [program] of
            Left tErr -> do
              putStr (show tErr)
              return Nothing
            Right as' -> do
              putStrLn "\n\nType Inference:"
              mapM_ print as'
              return $ Just Module {name = name, ast = ast, program = program}

visit :: Exp a -> IO (Exp a)
visit (App ann l r) = return (App ann r l)
visit e = return e

dumbScheme :: Qual Type -> Scheme
dumbScheme t = quantify (tv t) t

initialAssumption :: Definition SourceRange -> Assump
initialAssumption (Definition id (Just t) _) = id :>: t
initialAssumption (Definition id Nothing _) = id :>: dumbScheme ([] :=> TVar (tyvar "a"))

-- initialAssumptions :: [Syntax.TopLevel] -> [Assump]
-- initialAssumptions (v@(Syntax.Decl name val) : tls) = a : initialAssumptions tls
--   where
--     a = name :>: dumbScheme ([] :=> TVar (tyvar "a"))
-- initialAssumptions (v@(Syntax.TypeDecl name t) : ts) = (name :>: dumbScheme t) : initialAssumptions ts
-- initialAssumptions [] = []
