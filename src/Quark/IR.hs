module Quark.IR
  ( Type,
    Typeable,
  )
where

-- The Quark IR uses the same type object as the Futz frontend
import Futz.Types


-- An `a` is typable if it can be asked for it's type. This,
-- in Quark, must be simple, pure, and "constant time" :)
class Typeable a where
  typeOf :: a -> Type


-- All variables must have their type statically
-- assigned.
data Variable = Variable String Type

data Exp
  = Var Variable -- A simple varialbe
  | App Exp Exp -- Application
  | Lambda Variable Exp -- Expressions w/ a body



instance Typeable Variable where
  typeOf (Variable _ t) = t

instance Typeable Exp where
  typeOf (Var v) = typeOf v
  typeOf (App func _) = undefined -- TODO: extract the return value of the func
  typeOf (Lambda a body) = undefined


instance Show Variable where
  show (Variable v t) = show v <> "::" <> show t


instance Show Exp where
  show (Var v) = show v
  show (App f arg) = "(" <> show f <> " " <> show arg <> ")"
  show (Lambda arg body) = "(" <> "λ" <> show arg <> " -> " <> show body <> ")"
