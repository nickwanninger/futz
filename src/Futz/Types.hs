-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- -- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}



module Futz.Types (
  Type(..),
  TypeScheme(..)
)where

import Control.Monad
import Data.Foldable
import Data.List


type TVar = String
  

data Type = TNamed String [Type] -- A named type (like `Int`) and the arguments used to specify it
                                 -- ex: Maybe 'a will be one of these with one argument
          | TArrow Type Type     -- A lambda type
          | TVar String          -- A type variable
  deriving (Eq, Ord, Show)

infixr `TArrow`


-- A representation of `forall 'a 'b 'c . T`
data TypeScheme = TForAll [TVar] Type
  deriving (Eq, Ord, Show)
