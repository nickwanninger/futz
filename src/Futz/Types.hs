-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- -- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}



module Futz.Types where

import Control.Monad
import Data.Foldable
import Data.List


type TVar = String
data Type = TNamed String [Type]
          | TArrow Type Type
          | TVar String
  deriving (Eq, Ord)

infixr `TArrow`

instance Show Type where
  show (TNamed name args) = name <> ""
  show (TArrow a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TVar name) = "'" <> name


data TypeScheme = TForAll [TVar] Type
  deriving (Eq, Ord)

instance Show TypeScheme where
  show (TForAll vars t) = "forall " <> unwords (map ('\'' :) vars) <> " . " <> show t


builtinType :: String -> Type
builtinType s = TNamed s []


tInt :: Type
tInt = builtinType "Int"
