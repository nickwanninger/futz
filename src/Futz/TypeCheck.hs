module Futz.TypeCheck where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldr)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Futz.Syntax as S
import Futz.Types as T

-- TODO: this should support failure reasons
inferTop :: S.TopLevel -> (T.Type, [Constraint])
inferTop (TypeDecl name t) = undefined
inferTop (Decl name e) =
  case runState (inferExp e) defaultInferState of
    (t, s) -> (t, Set.toList $ constraints s)

data Constraint = EqConstraint Type Type -- two types are equal
  deriving (Eq, Ord)

instance Show Constraint where
  show (EqConstraint t1 t2) = show t1 <> " = " <> show t2

data InferState = InferState
  { constraints :: Set.Set Constraint,
    nextId :: Int,
    gamma :: Map.Map String T.Type
  }
  deriving (Eq, Show)

defaultInferState =
  InferState
    { constraints = Set.empty,
      nextId = 0,
      gamma = Map.empty
    }

-- the inference monad
type InferM a = State InferState a

-- make a new unique variable
newTypeVar :: InferM Type
newTypeVar = do
  ps <- get
  let id = nextId ps
  put $ ps {nextId = id + 1}
  return $ T.TVar ("a" <> show id)

addConstraint :: Constraint -> InferM ()
addConstraint c = do
  ps <- get
  put $ ps {constraints = Set.insert c $ constraints ps}

inferDumbBinary :: Exp -> Exp -> InferM T.Type
inferDumbBinary l r = do
  tL <- inferExp l
  tR <- inferExp r
  addConstraint $ EqConstraint tL tR
  addConstraint $ EqConstraint tR T.tInt
  addConstraint $ EqConstraint tL T.tInt
  return T.tInt

withDef :: S.Var -> T.Type -> InferM T.Type -> InferM T.Type
withDef name t f = do
  ps <- get -- get the previous state
  -- get the previous gamma, so we can restore it after
  let previousGamma = gamma ps
  modify $ \ps -> ps {gamma = Map.insert name t previousGamma}
  resType <- f
  modify $ \ps -> ps {gamma = previousGamma}

  return resType

inferExp :: S.Exp -> InferM T.Type
inferExp e = case e of
  -- TODO: this doesn't work with recursion. The type of the name needs
  --       to be available in the value's inferences!
  Let name value body -> do
    tValue <- newTypeVar
    tValue' <- withDef name tValue $ inferExp value
    addConstraint $ EqConstraint tValue tValue'
    withDef name tValue' $ inferExp body
  Plus l r -> inferDumbBinary l r
  Minus l r -> inferDumbBinary l r
  Times l r -> inferDumbBinary l r
  Div l r -> inferDumbBinary l r
  Negate v -> inferExp v
  Int i -> return T.tInt -- TODO: should check it's an int!
  Var name -> do
    st <- get
    maybe newTypeVar return (Map.lookup name $ gamma st)
  IfElse tst thn els -> do
    tstT <- inferExp tst
    thnT <- inferExp thn
    elsT <- inferExp els
    addConstraint $ EqConstraint thnT elsT
    -- return elsT
    return thnT
  Lambda x e -> do
    tArg <- newTypeVar -- make up a new value for the argument
    tReturn <- withDef x tArg $ inferExp e
    return $ T.TArrow tArg tReturn
  App f a -> do
    t <- newTypeVar
    t1 <- inferExp f
    t2 <- inferExp a
    addConstraint $ EqConstraint t1 $ T.TArrow t2 t
    return t

------------ Unification

-- type Subst = Map.Map T.TVar T.Type
data Subst = Subst T.TVar T.Type
  deriving (Eq, Show)

class Substitutable a where
  apply :: Subst -> a -> a

  -- TODO: I don't think we really need this yet.
  --       it's a function to find type variables
  ftv :: a -> Set.Set T.TVar

instance Substitutable T.Type where
  apply s (T.TNamed name args) = T.TNamed name $ Prelude.map (apply s) args
  apply s (T.TArrow a b) = T.TArrow (apply s a) (apply s b)
  -- apply s t@(T.TVar name) = Map.findWithDefault t name s
  apply (Subst fromName toType) t@(T.TVar name) = if fromName == name then toType else t

  ftv (T.TNamed _ []) = Set.empty -- optimization!
  ftv (T.TNamed _ args) = foldl Set.union Set.empty $ map ftv args
  ftv (T.TArrow a b) = ftv a `Set.union` ftv b
  ftv (T.TVar a) = Set.singleton a

instance Substitutable Constraint where
  apply s (EqConstraint a b) = EqConstraint (apply s a) (apply s b)

  ftv (EqConstraint a b) = ftv a `Set.union` ftv b

-- type Unify a = State ([Subst], [Constraint]) a
type Unify a = Except TypeError a

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type

unify :: [Constraint] -> Either TypeError [Subst]
unify c = runExcept (unifyM c)

unifyM :: [Constraint] -> Unify [Subst]
unifyM c = case c of
  [] -> return []
  (EqConstraint t1 t2) : cs -> case (t1, t2) of
    (T.TArrow a1 b1, T.TArrow a2 b2) -> unifyM $ EqConstraint a1 a2 : EqConstraint b1 b2 : cs
    (_, T.TVar var) -> do
      let sub = Subst var t1
      s <- unifyM $ subst sub cs
      return $ sub : s
    (T.TVar var, _) -> do
      let sub = Subst var t2
      s <- unifyM $ subst sub cs
      return $ sub : s
    (_, _) -> unifyM cs

subst :: Subst -> [Constraint] -> [Constraint]
subst s = map (apply s)
