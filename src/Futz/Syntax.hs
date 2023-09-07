module Futz.Syntax where

import qualified Futz.Types as T

type Var = String

-- Syntax expressions. These all evaluate to a value of some type.
data Exp
  = Let Var Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Negate Exp
  | Int Int
  | Var Var
  | IfElse Exp Exp Exp -- if ... then .. else ...
  | Lambda Var Exp -- Expressions w/ a body
  | App Exp Exp -- Application
  deriving (Eq)

instance Show Exp where
  show (Let name val body) = "let " <> name <> " = " <> show val <> " in " <> show body
  show (Plus l r) = show l <> " + " <> show r
  show (Minus l r) = show l <> " - " <> show r
  show (Times l r) = show l <> " * " <> show r
  show (Div l r) = show l <> " / " <> show r
  show (Negate v) = " -" <> show v
  show (Int i) = show i
  show (Var v) = v
  show (IfElse tst thn els) = "if " <> show tst <> " then " <> show thn <> " else " <> show els
  -- show (Lambda name body) = "(λ" <> name <> " -> " <> show body <> ")"
  show (Lambda a x) = "(λ" <> a <> showLambdaArg x <> ")" -- " -> " <> show body <> ")"
    where
      showLambdaArg (Lambda a x) = " " <> a <> showLambdaArg x
      showLambdaArg x = " -> " <> show x
  show (App f a) = "(" <> show f <> " " <> show a <> ")"

-- show _ = "incomplete"

data TopLevel
  = Decl Var Exp -- A top level declaration
  | TypeDecl Var T.Type --  A top level typing
  deriving (Eq)

instance Show TopLevel where
  show (Decl v e) = v <> " = " <> show e
  show (TypeDecl v t) = v <> " :: " <> show t <> " => " <> show (T.kind t)

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

type Program = [BindGroup]

-- The token type:
data Lexeme
  = LSyntax
  | LInt
  | LSym
  | LType
  | LTypeVar
  | LEq
  | LPlus
  | LMinus
  | LTimes
  | LDiv
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
