module Futz.Syntax where

import Data.List
import qualified Futz.Types as T

type Var = String

newtype Literal = LitInt Int
  deriving (Eq)

instance Show Literal where
  show (LitInt i) = show i

-- Syntax expressions. These all evaluate to a value of some type.
data Exp
  = Let Var Exp Exp
  | Lit Literal
  | Var Var
  | IfElse Exp Exp Exp -- if ... then .. else ...
  | Lambda Var Exp -- Expressions w/ a body
  | App Exp Exp -- Application
  | Inf Var Exp Exp -- An infix op (ex: `Inf + 1 2`)
  deriving (Eq)

opSymbols = "!#$%&*+./<=>?@\\^|-~"

showVar v
  | all (`elem` opSymbols) v = "(" <> v <> ")"
  | otherwise = v

instance Show Exp where
  show (Let name val body) = "let " <> name <> " = " <> show val <> " in " <> show body
  show (Lit l) = show l
  show (Var v) = showVar v
  show (IfElse tst thn els) = "if " <> show tst <> " then " <> show thn <> " else " <> show els
  -- show (Lambda name body) = "(λ" <> name <> " -> " <> show body <> ")"
  show (Lambda a x) = "(λ" <> a <> showLambdaArg x <> ")" -- " -> " <> show body <> ")"
    where
      showLambdaArg (Lambda a x) = " " <> a <> showLambdaArg x
      showLambdaArg x = " -> " <> show x
  show (App f a) = "(" <> show f <> " " <> show a <> ")"
  show (Inf op l r) = "(" <> show l <> " " <> op <> " " <> show r <> ")"

-- show _ = "incomplete"

data Constructor = Constructor T.Id [T.Type]
  deriving (Eq)

instance Show Constructor where
  show (Constructor id ts) = id <> " " <> unwords (map show ts)

data TopLevel
  = Decl Var Exp -- A top level declaration
  | TypeDecl Var T.Type --  A top level typing
  | DataDecl Var [T.Id] [Constructor]
  deriving (Eq)

instance Show TopLevel where
  show (Decl v e) = showVar v <> " = " <> show e
  show (TypeDecl v t) = showVar v <> " :: " <> show t <> " => " <> show (T.kind t)
  show (DataDecl name args ctors) = "data " <> name <> " " <> unwords args <> " = " <> intercalate " | " (map show ctors)

newtype Argument = Named Var
  deriving (Eq, Show)

newtype Pat = PVar Var
  deriving (Eq, Show)

data Impl = Impl Var [Binding]
  deriving (Eq, Show)

implName :: Impl -> Var
implName (Impl v _) = v

implBindings :: Impl -> [Binding]
implBindings (Impl _ as) = as

mergeImpls :: Impl -> [Impl] -> [Impl]
mergeImpls a@(Impl i bs) (b@(Impl i' bs') : is)
  | i == i' = Impl i (bs ++ bs') : is
  | otherwise = b : mergeImpls a is
mergeImpls a [] = [a]

findImpl :: [Impl] -> Var -> Maybe Impl
findImpl (i@(Impl name bs) : is) n = if name == n then Just i else findImpl is n
findImpl [] _ = Nothing

-- mergeImpl :: Impl -> [Impl] -> [Impl]
-- mergeImpl a@(Impl i bs) (b@(Impl i' bs') : is)
--   | i == i' = Impl i (bs++bs') : is
--   | otherwise = b : mergeImpls a is
-- mergeImpl a [] = [a]

-- An explicitly typed definition.
data Expl = Expl Var T.Scheme [Binding]
  deriving (Eq, Show)

-- A binding represents a particular definition of a value.
-- This can be `x = 4`, or `let f x = ... in ...`
data Binding = Binding [Pat] Exp
  deriving (Eq, Show)

bindingPatterns :: Binding -> [Pat]
bindingPatterns (Binding ps _) = ps

data BindGroup = BindGroup [Expl] [Impl]
  deriving (Eq, Show)

bgAddImpl :: BindGroup -> Var -> Binding -> BindGroup
bgAddImpl (BindGroup es is) id b = BindGroup es (mergeImpls (Impl id [b]) is)

bgAddExpl :: BindGroup -> Var -> T.Scheme -> BindGroup
bgAddExpl bg@(BindGroup es is) id t =
  case findImpl is id of
    Just i@(Impl _ bs) -> BindGroup (e : es) is'
      where
        is' = filter (== i) is
        e = Expl id t bs
    Nothing -> BindGroup (e : es) is
      where
        e = Expl id t []



-- TODO: this does not handle invalid programs.
fuseProgram :: [TopLevel] -> BindGroup
fuseProgram (t : ts) = case t of
  Decl name val -> bgAddImpl bg name (Binding [] val)
  TypeDecl name ty -> bgAddExpl bg name (T.quantify (T.tv ty) ([] T.:=> ty))
  DataDecl {} -> bg
  where
    bg = fuseProgram ts
fuseProgram [] = BindGroup [] []


type Program = [BindGroup]




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

-- A token is a Location in a file, a
-- class, and the raw string value
data Token
  = Tok Position Lexeme String
  | TStartOfLine
  | TEndOfFile
  deriving (Eq, Show)
