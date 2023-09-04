module Futz.Types where

import Control.Monad
import Data.Foldable
import Data.List

type TVar = String

data Type
  = TNamed String [Type]
  | TArrow Type Type
  | TVar String
  deriving (Eq, Ord)

infixr 9 `TArrow`

instance Show Type where
  show (TNamed name args) = name <> ""
  -- Arrow is a little interesting to print. For clairty we put
  -- parens around the argument if it itself is an Arrow, to be clear
  show (TArrow a b) = case a of
    (TArrow _ _) -> "(" <> show a <> ")" <> " -> " <> show b
    _ -> show a <> " -> " <> show b
  show (TVar name) = "'" <> name

data TypeScheme = ForAll [TVar] Type
  deriving (Eq, Ord)

instance Show TypeScheme where
  show (ForAll [] t) = "∀ . " <> show t
  show (ForAll vars t) = "∀" <> intercalate "" vars <> " . " <> show t

builtinType :: String -> Type
builtinType s = TNamed s []

tInt :: Type
tInt = builtinType "Int"
