module Futz.Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldr)
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Futz.Syntax
import Futz.Types
import Prelude hiding (foldr)

newtype TypeEnv = TypeEnv (Map.Map Var TypeScheme)
  deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv m) = intercalate "\n" $ map f entries
    where
      entries = Map.toList m
      f (name, scheme) = name <> " :: " <> show scheme

newtype Unique = Unique {count :: Int}

type Infer = ExceptT TypeError (State Unique)

type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  deriving (Show)

runInfer :: Infer (Subst, Type) -> Either TypeError TypeScheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> TypeScheme
closeOver (sub, ty) = normalize sc
  where
    sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique {count = 0}

extend :: TypeEnv -> (Var, TypeScheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe TypeScheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TNamed a _) = TNamed a [] -- TODO: constructor
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2

  ftv TNamed {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable TypeScheme where
  apply s (ForAll as t) = ForAll as $ apply s' t
    where
      s' = foldr Map.delete s as
  ftv (ForAll as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: Type -> Type -> Infer Subst
unify (l `TArrow` r) (l' `TArrow` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TNamed a _) (TNamed b _) | a == b = return nullSubst -- TODO: constructor
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t
  | t == TVar a = return nullSubst
  -- | occursCheck a t = throwError $ InfiniteType a t -- TODO: infinite type!
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar (letters !! count s)

instantiate :: TypeScheme -> Infer Type
instantiate (ForAll as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = ForAll as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

-- ops :: Binop -> Type
-- ops Add = typeInt `TArr` typeInt `TArr` typeInt
-- ops Mul = typeInt `TArr` typeInt `TArr` typeInt
-- ops Sub = typeInt `TArr` typeInt `TArr` typeInt
-- ops Eql = typeInt `TArr` typeInt `TArr` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

infer :: TypeEnv -> Exp -> Infer (Subst, Type)
infer env ex = case ex of
  Var x -> lookupEnv env x
  --
  Lambda x e -> do
    tv <- fresh
    let env' = env `extend` (x, ForAll [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArrow` t1)
  --
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArrow t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  --
  Let x e1 e2 -> do
    t0 <- fresh
    (s1, t1) <- infer (env `extend` (x, ForAll [] t0)) e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)

  IfElse cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (tInt `TArrow` tv `TArrow` tv `TArrow` tv)
  --
  -- Fix e1 -> do
  --   tv <- fresh
  --   inferPrim env [e1] ((tv `TArr` tv) `TArr` tv)

  -- Op op e1 e2 -> do
  --   inferPrim env [e1, e2] (ops op)

  (Int _)  -> return (nullSubst, tInt)
  -- Lit (LBool _) -> return (nullSubst, typeBool)
  _ -> return (nullSubst, tInt)

inferPrim :: TypeEnv -> [Exp] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
    inferStep (s, tf) exp = do
      (s', t) <- infer (apply s env) exp
      return (s' `compose` s, tf . TArrow t)

inferExpr :: TypeEnv -> Exp -> Either TypeError TypeScheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Exp)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex) : xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: TypeScheme -> TypeScheme
normalize (ForAll ts body) = ForAll (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TVar a) = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TNamed _ []) = [] -- TODO: constructors!
    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TNamed a _) = TNamed a [] -- TODO: constructors!
    normtype (TVar a) =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
