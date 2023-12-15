module Futz.Module where

import Control.Monad
import Control.Monad.Except
import Data.Either.Combinators
import Data.List
import Data.List.Unique
import Data.Text hiding (foldl, length, map, head)
import qualified Futz.Infer as Infer
import qualified Futz.Lexer as Lexer
import qualified Futz.Parser as Parser
import Futz.Syntax
import Futz.Types
import System.IO
import Text.Pretty.Simple

-- This is a representation of a logical `Module`, which
-- is effectively the internal data format of a parsed file.
data Module = Module
  { -- The name of the module
    name :: Text,
    -- The raw AST, in case it's useful to have it around
    ast :: [TopLevel SourceRange],
    -- Bindings
    program :: Program SourceRange,
    dependencies :: [String] -- TODO: figure out how to represent this!
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
  case Lexer.lex contents of
    -- If the lexer gives us an error, just print it and return Nothing. We
    -- need to make this load function return some kind of error, but I haven't
    -- decided on that representation yet.
    Left err -> do
      putStrLn err
      return Nothing
    -- If the tokenization works, parse the AST
    Right tokens -> do
      let env = exampleInsts initialEnv
      -- pPrint env
      -- mapM_ print tokens
      let ast = Parser.parseFutz tokens

      case validate ast of
        Left err -> do
          putStrLn ("Validation failed: " <> show err)
          return Nothing
        Right _ -> do
          mapM_ print ast
          -- Convert the top level AST nodes to a Program.
          let program = toProgram ast
          print program

          return $ Just Module {name = name, ast = ast, program = program, dependencies = []}

data ValidationError
  = DuplicateDefinition String
  | -- A toplevel definition has mismatched names in it's parts
    DefintionNameMismatch (TopLevel SourceRange)
  | -- A toplevel definition has too many types
    DefinitionTooManyTypes (TopLevel SourceRange)
  deriving (Eq, Show)

validateTopLevel :: TopLevel SourceRange -> Either ValidationError ()
validateTopLevel t = case t of
  -- TODO: validate a data declaration
  DataDecl {} -> return ()
  -- Validate a few things about top level function definitions
  TopDefn parts -> do
    -- if the number of unique names is not 1, then it is not valid.
    when (1 /= length (nub (map defnPartName parts))) (throwError $ DefintionNameMismatch t)
    when (numTypeDecls > 1) (throwError $ DefinitionTooManyTypes t)
    where
      -- There can be only one type declaration
      isTypeDecl (DefnType {}) = True
      isTypeDecl _ = False
      numTypeDecls = foldl (\a b -> a + fromEnum (isTypeDecl b)) 0 parts

validate :: [TopLevel SourceRange] -> Either ValidationError ()
validate ts = do
  mapM_ validateTopLevel ts
  mapM_ checkUnique names

  where getName :: TopLevel SourceRange -> Var
        getName (DataDecl name _ _) = name
        getName (TopDefn parts) = head (map defnPartName parts)

        names = map getName ts

        checkUnique :: Var -> Either ValidationError ()
        checkUnique name = case isUnique name names of
          Just True -> return ()
          _ -> throwError $ DuplicateDefinition name -- hmm.

  -- unless (allUnique names) (throwError

-- TODO: this does not handle invalid programs.
-- TODO: write a "validate" function, which ensures that syntax makes sense,
--       and variables are bound correctly.
toProgram :: [TopLevel a] -> Program a
toProgram (t : ts) = case t of
  DataDecl name targs ctors -> foldl (addDataCtor name targs) prog ctors
  TopDefn parts -> foldl addDefnPart prog parts
  where
    prog = toProgram ts

    addDefnPart :: Program a -> DefnPart a -> Program a
    addDefnPart p (DefnType id t) = progAttachScheme p id (quantify (tv t) t)
    addDefnPart p (DefnBind id b) = progAddBinding p id b
    -- Given a constructor, convert it to
    addDataCtor :: Id -> [Id] -> Program a -> Constructor -> Program a
    addDataCtor name targs p c@(Constructor id _) = progAddDefinition p id (generateConstructor name targs c)
toProgram [] = Program {programDefs = [], programClasses = []}

-- initialAssumptions :: [Syntax.TopLevel] -> [Assump]
-- initialAssumptions (v@(Syntax.Decl name val) : tls) = a : initialAssumptions tls
--   where
--     a = name :>: dumbScheme ([] :=> TVar (tyvar "a"))
-- initialAssumptions (v@(Syntax.TypeDecl name t) : ts) = (name :>: dumbScheme t) : initialAssumptions ts
-- initialAssumptions [] = []
