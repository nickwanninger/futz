module Futz.Syntax where

import qualified Futz.Types as T


type Var = String

-- Syntax expressions. These all evaluate to a value of some type.
data Exp = Let Var Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Int Int
         | Var Var
         | Lambda Var Exp -- Expressions w/ a body
         | App Exp Exp       -- Application
  deriving (Eq, Show)

data TopLevel = Decl Var Exp    -- A top level declaration
              | TypeDecl Var T.Type --  A top level typing
  deriving (Eq, Show)


newtype Argument = Named Var
  deriving (Eq, Show)

-- The token type:
data Lexeme = LSyntax
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
  deriving (Eq,Show)


-- A line, column position (1 indexed)
data Position = Pos Int Int
  deriving (Eq, Show)

-- A token is a Location in a file, a
-- class, and the raw string value
data Token = Tok Position Lexeme String
           | TStartOfLine
           | TEndOfFile
  deriving (Eq, Show)
