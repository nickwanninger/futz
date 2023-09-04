module Futz.TypeCheck where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (chr, ord)
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

-- This function is an entrypoint to the type inference system, and
-- returns the type scheme used to describe the type of some expression
inferType :: S.Exp -> Either TypeError T.TypeScheme
inferType exp =
  case runState (inferExp exp) defaultInferState of
    (t, s) ->
      let unification = unify (Set.toList $ constraints s)
       in case unification of
            Left err -> Left err
            Right unification -> Right (normalize (generalize (foldl (flip apply) t unification)))

-- Right $ T.ForAll [] T.tInt

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

idToTypeVarName :: Int -> T.TVar
idToTypeVarName id =
  if (id + ord 'a') > ord 'z'
    then "t" <> show id
    else [chr $ ord 'a' + id]

-- make a new unique variable
fresh :: InferM Type
fresh = do
  ps <- get
  let id = nextId ps
  put $ ps {nextId = id + 1}
  return $ T.TVar $ idToTypeVarName id

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
    tValue <- fresh
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
    maybe fresh return (Map.lookup name $ gamma st)
  IfElse tst thn els -> do
    tstT <- inferExp tst
    thnT <- inferExp thn
    elsT <- inferExp els
    addConstraint $ EqConstraint thnT elsT
    -- return elsT
    return thnT
  Lambda x e -> do
    tArg <- fresh -- make up a new value for the argument
    tReturn <- withDef x tArg $ inferExp e
    return $ T.TArrow tArg tReturn
  App f a -> do
    t <- fresh
    t1 <- inferExp f
    t2 <- inferExp a
    addConstraint $ EqConstraint t1 $ T.TArrow t2 t
    return t

generalize :: T.Type -> T.TypeScheme
generalize t = T.ForAll as t
  where
    as = Set.toList $ ftv t

-- newtype TypeEnv = TypeEnv (Map.Map Var T.TypeScheme)
--
-- lookupEnv :: TypeEnv -> Var -> InferM (Subst, Type)
-- lookupEnv (TypeEnv env) x =
--   case Map.lookup x env of
--     Nothing -> undefined -- TODO: Undefined variable
--     Just s  -> do t <- instantiate s
--                   return (nullSubst, t)
--
-- w :: TypeEnv -> Exp -> InferM (Subst, T.Type)
-- w env e = undefined


--
--
--
--
--
--
--
--
--
--
--
--
--
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
  deriving (Eq, Show)

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


letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']
normalize :: T.TypeScheme -> T.TypeScheme
normalize (T.ForAll ts body) = T.ForAll (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (T.TVar a)   = [a]
    fv (T.TArrow a b) = fv a ++ fv b
    fv (T.TNamed _ [])   = [] -- TODO: constructors!

    normtype (T.TArrow a b) = T.TArrow (normtype a) (normtype b)
    normtype (T.TNamed a _)   = TNamed a [] -- TODO: constructors!
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> T.TVar x
        Nothing -> error "type variable not in signature"
