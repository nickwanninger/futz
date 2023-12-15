{-# LANGUAGE InstanceSigs #-}

module Futz.Syntax where

import Control.Lens
import Data.Foldable
import Data.List (intercalate)
import Data.Traversable
-- The futz packages
import Futz.Types

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
  | NativeCall a Var Type [Exp a] -- A native, uncurried, magic runtime call. The return type given is trusted, and the types of the arguments must be concrete.
  | Match a (Exp a) [MatchArm a]
  | Do a [DoLine a] -- Monadic do notation
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
  show (Match _ e arms) = "match " <> show e <> " { " <> intercalate "; " (map show arms) <> " }"
  show (Do _ lines) = "do { " <> intercalate "; " (map show lines) <> " }"

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

data MatchArm a = MatchArm Pat (Exp a)
  deriving (Eq)

instance Show (MatchArm a) where
  show (MatchArm p e) = show p <> " => " <> show e

-------------------------------------------------------------------------------

-- A line in a do block
data DoLine a
  = DLBind a Var (Exp a) -- an expression with a binding (>>=)
  | DLExp a (Exp a) -- an expression w/o binding (>>)
  deriving (Eq)

instance Show (DoLine a) where
  show (DLBind _ name exp) = name <> " <- " <> show exp
  show (DLExp _ exp) = show exp

desugarDo :: Exp a -> Exp a
-- Terminator line
desugarDo (Do a [l]) = case l of
  -- do { e; ls }  ->   e >> do { ls }
  DLExp a e -> e
-- TODO

-- Non-terminator Line
desugarDo (Do a (l : ls)) = case l of
  -- do { e; ls }  ->   e >> do { ls }
  DLExp a e -> Inf a ">>" e (desugarDo $ Do a ls)
  -- do { n <- e; ls }   ->   e >>= (fn n -> do { ls })
  DLBind a n e -> Inf a ">>=" e (Lambda a n (desugarDo $ Do a ls))
desugarDo x = x

-------------------------------------------------------------------------------

data Constructor = Constructor Id [Type]
  deriving (Eq)

instance Show Constructor where
  show (Constructor id ts) = id <> " " <> unwords (map show ts)

data TopLevel a
  = TopDefn [DefnPart a]
  | DataDecl Id [Id] [Constructor]
  -- | ClassDecl a (Class) -- Tycon Tyvar [(Var, Qual Type)]
  deriving (Eq)

data DefnPart a
  = DefnType Var (Qual Type)
  | DefnBind Var (Binding a)
  deriving (Eq)

defnPartName :: DefnPart a -> Var
defnPartName (DefnType v _) = v
defnPartName (DefnBind v _) = v

instance Show (DefnPart a) where
  show (DefnType id t) = id <> " :: " <> show t
  show (DefnBind id b) = id <> " " <> show b

instance Show (TopLevel a) where
  show :: TopLevel a -> String
  show (DataDecl name args ctors) = "data " <> name <> " " <> unwords args <> " = " <> intercalate " | " (map show ctors)
  show (TopDefn parts) = "def " <> intercalate "\n  | " (map show parts)

newtype Argument = Named Var
  deriving (Eq, Show)

data Pat
  = PVar Var -- Not a pattern, simple match
  | PWildcard -- wildcard: `_`
  | PAs Var Pat -- binding + match: `a@(...)`
  | PCon Assump [Pat] -- constructor matching: `Ctor a b ...`
  deriving (Eq)

instance Show Pat where
  show (PVar v) = v
  show PWildcard = "_"
  show (PAs id pat) = id <> "@" <> show pat
  show (PCon (id :>: _) []) = id
  show (PCon (id :>: _) pats) = id <> " " <> unwords (map show pats)

-- A typed definition of some binding. The type can be `Nothing`,
-- which indicates it must be inferred.
data Definition a = Definition Var (Maybe Scheme) [Binding a]
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
-- This will return the type `Type`, and will trust that the constructor
-- arguments have been verified and are correct. No internal validation
-- is performed.
generateConstructor :: Id -> [Id] -> Constructor -> Definition a
generateConstructor name targs c@(Constructor id args) = def
  where
    -- We will allow type inference to choose the type of this function.
    def = Definition id (Just (quantify (tv ft) ([] :=> ft))) []
    rt = fromDataDecl name targs -- return type
    -- Generate a function type for this definition type
    ft = foldr fn rt args

-------------------------------

-- Given a program, find (or create) a definition of a given name,
-- and modify it with a provided function
progModDef :: Program a -> Var -> (Definition a -> Definition a) -> Program a
progModDef prog id f = prog {programDefs = mod (programDefs prog)}
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

progAttachScheme :: Program a -> Var -> Scheme -> Program a
progAttachScheme prog id sc = progModDef prog id mod
  where
    mod (Definition name _ bs) = Definition name (Just sc) bs


-- A program is just the data representation of the contents of a module. The
-- reason this isn't called "Module" is because modules are loaded independently,
-- then renaming is applied, and they are later all merged into a single "Program".
-- Before this, each module has it's own "Program" -- think of it as a "Subprogram"
data Program a = Program
  { -- The list of definitions in this program
    programDefs :: [Definition a],
    -- A list of the classes defined in this program
    programClasses :: [Class]
  }
  deriving (Eq, Show)

-- The token type:
data Lexeme
  = LSyntax
  | LStartLayout -- A token which indicates that a layout will start after it
  | LEndLayout -- A token which indicates that a layout will end before it
  | LInt
  | LSym
  | LType
  | LTypeVar
  | LEq
  | LOp -- operator (symbols like +, -, etc)
  | LLParen
  | LRParen
  | LIsType
  | LArrow
  | LLambda
  | LOf
  | LPipe
  | LOpen -- {
  | LClose -- }
  | LSemi -- ;
  | LEOF -- an end of file token
  | LNewline -- This gets removed by the postprocessing pass
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
  | VTok Lexeme -- A virtual token, which was not in the source code
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
  locate :: Exp SourceRange -> SourceRange
  locate (Let r _ _) = r
  locate (Lit r _) = r
  locate (Var r _) = r
  locate (IfElse r _ _ _) = r
  locate (Lambda r _ _) = r
  locate (App r _ _) = r
  locate (Inf r _ _ _) = r
  locate (NativeCall r _ _ _) = r

instance Locatable a => Locatable [a] where
  locate ls = mergeRange (locate $ head ls) (locate $ last ls)

instance Locatable (MatchArm SourceRange) where
  locate (MatchArm p e) = locate e -- mergeRange (locate p) (locate e)

instance Locatable (DoLine SourceRange) where
  locate (DLBind a _ _) = a
  locate (DLExp a _) = a
