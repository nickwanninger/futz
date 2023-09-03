module Syntax where

import qualified Types as T


-- Syntax expressions. These all evaluate to a value of some type.
data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Int Int
         | Var String
         | Lambda String Exp -- Expressions w/ a body
         | App Exp Exp       -- Application
  deriving (Eq, Show)

data TopLevel = Decl String Exp    -- A top level declaration
              | TypeDecl String T.Type --  A top level typing
  deriving (Eq, Show)


newtype Argument = Named String
  deriving (Eq, Show)

-- The token type:
data Lexeme = LSyntax
            | LInt
            | LSym
            | LType
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
