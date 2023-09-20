module Quark.IR
  ( Type,
    Typeable,
  )
where

data Type

-- An `a` is typable if it can be asked for it's type. This,
-- in Quark, must be simple, pure, and "constant time" :)
class Typeable a where
  typeOf :: a -> Type