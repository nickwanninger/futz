{-# LANGUAGE InstanceSigs #-}

module Futz.Syntax where

import Control.Lens
import Data.Foldable
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
  = Let a [Definition a] (Exp a)
  | Lit a Literal
  | Var a Var
  | IfElse a (Exp a) (Exp a) (Exp a) -- if ... then .. else ...
  | Lambda a Var (Exp a) -- Expressions w/ a body
  | App a (Exp a) (Exp a) -- Application
  | Inf a Var (Exp a) (Exp a) -- An infix op (ex: `Inf + 1 2`)
  | NativeCall a Var T.Type [Exp a] -- A native, uncurried, magic runtime call. The return type given is trusted, and the types of the arguments must be concrete.
  deriving (Eq)

-- makePrisms ''Exp

opSymbols :: String
opSymbols = "!#$%&*+./<=>?@\\^|-~"

showVar :: [Char] -> String
showVar v
  | all (`elem` opSymbols) v = "(" <> v <> ")"
  | otherwise = v

instance Show (Exp a) where
  -- show (Let _ binds body) = "let " <> name <> " = " <> show val <> " in " <> show body
  show (Let _ defs body) = "let " <> intercalate " | " (map showDef defs) <> " in " <> show body
    where
      showDef :: Definition a -> String
      showDef (Definition name _ [binding]) = name <> " " <> show binding
  show (Lit _ l) = show l
  show (Var _ v) = showVar v
  show (IfElse _ tst thn els) = "if " <> show tst <> " then " <> show thn <> " else " <> show els
  show (Lambda _ a x) = "(λ" <> a <> showLambdaArg x <> ")" -- " -> " <> show body <> ")"
    where
      showLambdaArg (Lambda _ a x) = " " <> a <> showLambdaArg x
      showLambdaArg x = " -> " <> show x
  show (App _ f a) = "(" <> show f <> " " <> show a <> ")"
  show (Inf _ op l r) = "(" <> show l <> " " <> op <> " " <> show r <> ")"
  show (NativeCall _ name t args) = "#(" <> name <> " :: " <> show t <> " | " <> intercalate " | " (map show args) <> ")"

-- Apply a transformation function to each expression in an expression tree
visitExp :: Monad m => (Exp a -> m (Exp a)) -> Exp a -> m (Exp a)
visitExp f e = case e of
  Let ann bindings body -> do
    -- val' <- recur val
    bindings' <- mapM (visitDef f) bindings
    body' <- recur body
    f (Let ann bindings' body')
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
  where
    recur = visitExp f

visitDef :: Monad m => (Exp a -> m (Exp a)) -> Definition a -> m (Definition a)
visitDef f = return -- TODO! This is a NO-OP

data Constructor = Constructor T.Id [T.Type]
  deriving (Eq)

instance Show Constructor where
  show (Constructor id ts) = id <> " " <> unwords (map show ts)

data TopLevel a
  = TopDefn [DefnPart a]
  | DataDecl T.Id [T.Id] [Constructor]
  deriving (Eq)

data DefnPart a
  = DefnType Var (T.Qual T.Type)
  | DefnBind Var (Binding a)
  deriving (Eq)

instance Show (DefnPart a) where
  show (DefnType id t) = id <> " :: " <> show t
  show (DefnBind id b) = id <> show b

instance Show (TopLevel a) where
  show :: TopLevel a -> String
  show (DataDecl name args ctors) = "data " <> name <> " " <> unwords args <> " = " <> intercalate " | " (map show ctors)
  show (TopDefn parts) = "def " <> intercalate "\n  | " (map show parts)

newtype Argument = Named Var
  deriving (Eq, Show)

newtype Pat = PVar Var
  deriving (Eq)

instance Show Pat where
  show (PVar v) = v

-- A typed definition of some binding. The type can be `Nothing`,
-- which indicates it must be inferred.
data Definition a = Definition Var (Maybe T.Scheme) [Binding a]
  deriving (Eq, Show)

-- A binding represents the pattern matching on the left side, and the
-- resulting expression on the right side of an equals sign
data Binding a = Binding [Pat] (Exp a)
  deriving (Eq)

instance Show (Binding a) where
  show (Binding ps body) = unwords (map show ps) <> " = " <> show body

bindingPatterns :: Binding a -> [Pat]
bindingPatterns (Binding ps _) = ps

-- Convert a constructor of a data definition to a list of
-- definitions, which are internally calls to the native allocation function.
-- This will return the type `T.Type`, and will trust that the constructor
-- arguments have been verified and are correct. No internal validation
-- is performed.
generateConstructor :: T.Id -> [T.Id] -> Constructor -> Definition a
generateConstructor name targs c@(Constructor id args) = def
  where
    -- We will allow type inference to choose the type of this function.
    def = Definition id (Just (T.quantify (T.tv ft) ([] T.:=> ft))) []
    rt = T.fromDataDecl name targs -- return type
    -- Generate a function type for this definition type
    ft = foldr T.fn rt args

-- args = zipWith (\_ i -> Var nullSourceRange ("a" <> show i)) args [0 ..]
-- pats = []
-- body = NativeCall nullSourceRange "alloc" t args

-------------------------------

-- Given a program, find (or create) a definition of a given name,
-- and modify it with a provided function
progModDef :: Program a -> Var -> (Definition a -> Definition a) -> Program a
progModDef prog id f = prog {defs = mod (defs prog)}
  where
    -- mod :: [Definition a] -> [Definition a]
    mod (d@(Definition id' _ _) : ds)
      | id == id' = f d : ds
      | otherwise = d : mod ds
    mod [] = [f (Definition id Nothing [])]

progAddDefinition :: Program a -> Var -> Definition a -> Program a
progAddDefinition prog id d = progModDef prog id mod
  where
    mod (Definition name t bs) = d

progAddBinding :: Program a -> Var -> Binding a -> Program a
progAddBinding prog id b = progModDef prog id mod
  where
    mod (Definition name t bs) = Definition name t (b : bs)

progAttachScheme :: Program a -> Var -> T.Scheme -> Program a
progAttachScheme prog id sc = progModDef prog id mod
  where
    mod (Definition name _ bs) = Definition name (Just sc) bs

-- TODO: this does not handle invalid programs.
-- TODO: write a "validate" function, which ensures that syntax makes sense,
--       and variables are bound correctly.
fuseProgram :: [TopLevel a] -> Program a
fuseProgram (t : ts) = case t of
  DataDecl name targs ctors -> foldl (addDataCtor name targs) prog ctors
  TopDefn parts -> foldl addDefnPart prog parts
  where
    prog = fuseProgram ts

    addDefnPart :: Program a -> DefnPart a -> Program a
    addDefnPart p (DefnType id t) = progAttachScheme p id (T.quantify (T.tv t) t)
    addDefnPart p (DefnBind id b) = progAddBinding p id b
    -- Given a constructor, convert it to
    addDataCtor :: T.Id -> [T.Id] -> Program a -> Constructor -> Program a
    addDataCtor name targs p c@(Constructor id _) = progAddDefinition p id (generateConstructor name targs c)
fuseProgram [] = Program {defs = []}

newtype Program a = Program {defs :: [Definition a]}
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
  | LLCurly
  | LRCurly
  | LArrow
  | LLambda
  | LOf
  | LPipe
  | LIsType
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- A line, column position (1 indexed)
data Position = Pos Int Int
  deriving (Eq)

nullPosition = Pos 0 0

instance Show Position where
  show (Pos line col) = "line " <> show line <> " col " <> show col

data SourceRange = SourceRange Position Position
  deriving (Eq)

nullSourceRange = SourceRange nullPosition nullPosition

instance Show SourceRange where
  show (SourceRange a b) = show a <> " - " <> show b

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
  locate (Tok (Pos line col) _ s) = SourceRange (Pos line col) (Pos line (col + Prelude.length s))

instance Locatable (Exp SourceRange) where
  locate (Let r _ _) = r
  locate (Lit r _) = r
  locate (Var r _) = r
  locate (IfElse r _ _ _) = r
  locate (Lambda r _ _) = r
  locate (App r _ _) = r
  locate (Inf r _ _ _) = r
  locate (NativeCall r _ _ _) = r
