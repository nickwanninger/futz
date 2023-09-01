module Syntax where

-- The token type:
data Token = TLet
           | TIn
           | TInt Int
           | TSym String
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
  deriving (Eq,Show)


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

data Argument = Named String
  deriving Show


expandLambdaArguments :: [Argument] -> Exp -> Exp
expandLambdaArguments (x:[]) body = Lambda x body
expandLambdaArguments (x:xs) body = Lambda x (expandLambdaArguments xs body)
