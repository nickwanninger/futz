module Futz.Module where

import Data.Text
import qualified Futz.Infer as Infer
import qualified Futz.Lexer as Lexer
import qualified Futz.Parser as Parser
import qualified Futz.Syntax as Syntax
import Futz.Types
import System.IO

-- This is a representation of a logical `Module`, which
-- is effectively the internal data format of a parsed file.
data Module = Module
  { -- The name of the module
    name :: Text,
    -- The raw AST, in case it's useful to have it around
    ast :: [Syntax.TopLevel],
    -- Bindings
    bindings :: Syntax.BindGroup
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
      let ast = Parser.parseFutz tokens
      mapM_ print ast
      let bindings = Syntax.fuseProgram ast
      let env = exampleInsts initialEnv
      case env of
        Just env -> do
          let as = initialAssumptions ast
          print bindings
          let as' = Infer.tiProgram env [] [bindings]
          mapM_ print as'
          return $ Just Module {name = name, ast = ast, bindings = bindings}
        Nothing -> do
          return Nothing

dumbScheme :: Type -> Scheme
dumbScheme t = quantify (tv t) ([] :=> t)

initialAssumptions :: [Syntax.TopLevel] -> [Assump]
initialAssumptions (v@(Syntax.Decl name val) : tls) = a : initialAssumptions tls
  where
    a = name :>: dumbScheme (TVar (tyvar "a"))
initialAssumptions (v@(Syntax.TypeDecl name t) : ts) = (name :>: dumbScheme t) : initialAssumptions ts
initialAssumptions [] = []
