
module Futz.TypeCheck where

import Futz.Types as T
import Futz.Syntax as S

import Control.Monad.State
import Control.Monad.Except

import Data.Monoid
import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (Map.Map S.Var T.TypeScheme)

