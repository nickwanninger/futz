{-# LANGUAGE InstanceSigs #-}
module Futz.Syntax where

import Control.Lens
import Data.List (intercalate)
import Data.Traversable
import qualified Futz.Types as T

type Var = String


newtype Literal = LitInt Int
  deriving (Eq)

instance Show Literal where
  show (LitInt i) = show i

class Visitable a where
  visit :: Monad m => a -> (a -> m a) -> m a

-- Syntax expressions. These all evaluate to a value of some type.
data Exp a
  = Let a Var (Exp a) (Exp a)
  | Lit a Literal
  | Var a Var
  | IfElse a (Exp a) (Exp a) (Exp a) -- if ... then .. else ...
  | Lambda a Var (Exp a) -- Expressions w/ a body
  | App a (Exp a) (Exp a) -- Application
  | Inf a Var (Exp a) (Exp a) -- An infix op (ex: `Inf + 1 2`)
  deriving (Eq)

makePrisms ''Exp

opSymbols :: String
opSymbols = "!#$%&*+./<=>?@\\^|-~"

showVar :: [Char] -> String
showVar v
  | all (`elem` opSymbols) v = "(" <> v <> ")"
  | otherwise = v

instance Show (Exp a) where
  show (Let _ name val body) = "let " <> name <> " = " <> show val <> " in " <> show body
  show (Lit _ l) = show l
  show (Var _ v) = showVar v
  show (IfElse _ tst thn els) = "if " <> show tst <> " then " <> show thn <> " else " <> show els
  show (Lambda _ a x) = "(λ" <> a <> showLambdaArg x <> ")" -- " -> " <> show body <> ")"
    where
      showLambdaArg (Lambda _ a x) = " " <> a <> showLambdaArg x
      showLambdaArg x = " -> " <> show x
  show (App _ f a) = "(" <> show f <> " " <> show a <> ")"
  show (Inf _ op l r) = "(" <> show l <> " " <> op <> " " <> show r <> ")"

-- Apply a transformation function to each expression in an expression tree
visitExp :: Monad m => (Exp a -> m (Exp a)) -> Exp a -> m (Exp a)
visitExp f e = case e of
  Let ann name val body -> do
    val' <- recur val
    body' <- recur body
    f (Let ann name val' body')
  IfElse ann tst thn els -> do
    tst' <- recur tst
    thn' <- recur thn
    els' <- recur els
    f (IfElse ann tst' thn' els')
  Lambda ann a x -> do
    x' <- recur x
    f (Lambda ann a x')
  App ann a b -> do
    a' <- recur a
    b' <- recur b
    f (App ann a' b')
  Inf ann op a b -> do
    a' <- recur a
    b' <- recur b
    f (Inf ann op a' b')
  -- This handles all the simple cases (var, lit, etc)
  _ -> f e
  where recur = visitExp f

data Constructor = Constructor T.Id [T.Type]
  deriving (Eq)

instance Show Constructor where
  show (Constructor id ts) = id <> " " <> unwords (map show ts)

data TopLevel
  = Decl Var (Exp SourceRange) -- A top level declaration
  | TypeDecl Var (T.Qual T.Type) --  A top level typing
  | DataDecl Var [T.Id] [Constructor]
  deriving (Eq)

instance Show TopLevel where
  show (Decl v e) = showVar v <> " = " <> show e
  show (TypeDecl v t) = showVar v <> " :: " <> show t
  show (DataDecl name args ctors) = "data " <> name <> " " <> unwords args <> " = " <> intercalate " | " (map show ctors)

newtype Argument = Named Var
  deriving (Eq, Show)

newtype Pat = PVar Var
  deriving (Eq, Show)

-- A typed definition of some binding. The type can be `Nothing`,
-- which indicates it must be inferred.
data Definition = Definition Var (Maybe T.Scheme) [Binding]
  deriving (Eq, Show)

-- A binding represents the pattern matching on the left side, and the
-- resulting expression on the right side of an equals sign
data Binding = Binding [Pat] (Exp SourceRange)
  deriving (Eq, Show)

bindingPatterns :: Binding -> [Pat]
bindingPatterns (Binding ps _) = ps


-- Given a program, find (or create) a definition of a given name,
-- and modify it with a provided function
progModDef :: Program -> Var -> (Definition -> Definition) -> Program
progModDef prog id f = prog { defs = mod (defs prog) }
  where mod :: [Definition] -> [Definition] 
        mod (d@(Definition id' _ _):ds)
          | id == id' = f d : ds
          | otherwise = d : mod ds
        mod [] = [f (Definition id Nothing [])]

progAddBinding :: Program -> Var -> Binding -> Program
progAddBinding prog id b = progModDef prog id mod
  where mod (Definition name t bs) = Definition name t (b:bs)


progAttachScheme :: Program -> Var -> T.Scheme -> Program
progAttachScheme prog id sc = progModDef prog id mod
  where mod (Definition name _ bs) = Definition name (Just sc) bs


-- TODO: this does not handle invalid programs.
fuseProgram :: [TopLevel] -> Program
fuseProgram (t : ts) = case t of
  Decl name val -> progAddBinding prog name (Binding [] val)
  TypeDecl name ty -> progAttachScheme prog name (T.quantify (T.tv ty) ty)
  DataDecl {} -> prog
  where
    prog = fuseProgram ts
fuseProgram [] = Program { defs = [] }

newtype Program = Program { defs :: [Definition] }
  deriving (Eq, Show)

-- The token type:
data Lexeme
  = LSyntax
  | LInt
  | LSym
  | LType
  | LTypeVar
  | LEq
  | LOp -- operator (symbols like +, -, etc)
  | LLParen
  | LRParen
  | LArrow
  | LLambda
  | LOf
  | LPipe
  | LIsType
  deriving (Eq, Show)

-- A line, column position (1 indexed)
data Position = Pos Int Int
  deriving (Eq, Show)

  
data SourceRange = SourceRange Position Position
  deriving (Eq, Show)

mergeRange :: SourceRange -> SourceRange -> SourceRange
mergeRange (SourceRange start _) (SourceRange _ end) = SourceRange start end

-- A token is a Location in a file, a
-- class, and the raw string value
data Token
  = Tok Position Lexeme String
  | TStartOfLine
  | TEndOfFile
  deriving (Eq, Show)

tokVal :: Token -> String
tokVal (Tok _ _ s) = s
tokVal _ = "???"


class Locatable a where
  locate :: a -> SourceRange

instance Locatable Token where
  locate (Tok (Pos line col) _ s) = SourceRange (Pos line col) (Pos line (col + length s))

instance Locatable (Exp SourceRange) where
  locate (Let r _ _ _) = r
  locate (Lit r _) = r
  locate (Var r _) = r
  locate (IfElse r _ _ _) = r
  locate (Lambda r _ _) = r
  locate (App r _ _) = r
  locate (Inf r _ _ _) = r