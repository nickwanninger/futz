module Syntax where


-- Syntax expressions
data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Int Int
         | Var String
         | Lambda Argument Exp -- Expressions w/ a body
         | App Exp Exp       -- Application
  deriving Show

newtype Argument = Named String
  deriving Show

-- The token type:
data TokenClass = TSyntax
                | TInt
                | TSym
                | TEq
                | TPlus
                | TMinus
                | TTimes
                | TDiv
                | TLParen
                | TRParen
                | TArrow
                | TLambda
                | TOf
                | TEOF
  deriving (Eq,Show)


-- A line, column position (1 indexed)
data Position = Pos Int Int
  deriving (Eq, Show)

-- A token is a Location in a file, a
-- class, and the raw string value
data Token = Tok Position TokenClass String
  deriving (Eq, Show)
