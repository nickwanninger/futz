{-# HLINT ignore "Use zipWithM" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Futz.Infer where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (ap, liftM)
import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import Futz.Syntax
import Futz.Types

data Expl = Expl Var Scheme [Binding SourceRange]
  deriving (Eq, Show)

data Impl = Impl Var [Binding SourceRange]
  deriving (Eq, Show)

implName (Impl v _) = v

implBindings (Impl _ as) = as

-- The Type Inference Monad
type TI a = ExceptT TypeError (State InferState) a

data InferState = InferState
  { substs :: Subst,
    nextId :: Int,
    mappings :: Map.Map (Exp SourceRange) Type
  }
  deriving (Eq, Show)

defaultInferState = InferState {substs = nullSubst, nextId = 0, mappings = Map.empty}

runTI :: TI a -> Either TypeError a
runTI ti = evalState (runExceptT ti) defaultInferState

-- evalState (runExceptT ti) defaultInferState

-- The getSubst operation returns the current substitution, while unify extends it with a most
-- general unifier of its arguments:

getSubst :: TI Subst
getSubst = do
  (lift . gets) substs

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  let t1' = apply s t1
  let t2' = apply s t2
  u <- liftEither $ mgu t1' t2'
  extSubst u

unifyList :: [Type] -> TI ()
unifyList [] = return ()
unifyList [_] = return ()
unifyList (t1 : t2 : ts) =
  do
    unify t1 t2
    unifyList (t2 : ts)

-- For clarity, we define the operation that extends the substitution as a separate function,
-- even though it is used only here in the definition of unify:
extSubst :: Subst -> TI ()
extSubst s' = do
  (lift . Control.Monad.State.modify) (\st -> st {substs = s' @@ substs st})

-- TI (\s n -> (s' @@ s, n, ()))

-- Overall, the decision to hide the current substitution in the TI monad makes the presentation
-- of type inference much clearer. In particular, it avoids heavy use of apply every time an
-- extension is (or might have been) computed.
--
-- There is only one primitive that deals with the integer portion of the state, using it in
-- combination with enumId to generate a new type variable of a specified kind:
--
newIdNum :: TI Int
newIdNum = do
  ps <- get
  let id = nextId ps
  put $ ps {nextId = id + 1}
  return id

-- TI (\s n -> (s, n + 1, n))

newTVar :: Kind -> TI Type
newTVar k = do
  n <- newIdNum
  return $ TVar (Tyvar (enumId n) k)

-- One place where newTVar is useful is in instantiating a type scheme
-- with new type variables of appropriate kinds:
freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return (inst ts qt)

-- The structure of this definition guarantees that ts has exactly the right number of type
-- variables, and each with the right kind, to match ks. Hence, if the type scheme is well-formed,
-- then the qualified type returned by freshInst will not contain any unbound generics of the form
-- TGen n. The definition relies on an auxiliary function inst, which is a variation of apply that
-- works on generic variables. In other words, inst ts t replaces each occurrence of a generic variable
-- TGen n in t with ts!!n. It is convenient to build up the definition of inst using overloading:

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n) = ts !! n
  inst ts t = t

instance Instantiate a => Instantiate [a] where
  inst :: Instantiate a => [Type] -> [a] -> [a]
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-------------------------------------------------------------------------------

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

class Inferrable e where
  infer :: Infer e Type

makeExpressionTypeError :: Exp SourceRange -> String -> TypeError
makeExpressionTypeError exp msg =
  TEGeneric $
    unlines
      [ "Type error at " <> show (locate exp) <> ":",
        "   " <> msg,
        "   Problematic Expression:",
        "       " <> show exp
      ]

-- Expressions
-- Next we describe type inference for expressions, represented by the Expr datatype:
tiExpr :: Infer (Exp SourceRange) Type
tiExpr ce as e = case e of
  -- let, supporting rec
  Let _ defs e -> do
    -- (ps, as') <- tiDefinitions ce as [Definition n Nothing [Binding [] v]]
    -- (ps, as') <- tiBindings ce as bindings t
    (ps, as') <- tiDefinitions ce as defs

    (qs, t) <- tiExpr ce (as' ++ as) e
    return (ps ++ qs, t)

  -- type infer literal integers
  Lit _ (LitInt _) -> do
    v <- newTVar Star
    return ([IsIn "Num" v], v)

  -- variable inference is just a lookup
  Var _ i -> case findAssump i as of
    Just sc -> do
      (ps :=> t) <- freshInst sc
      return (ps, t)
    Nothing -> throwError $ TEGeneric ("Failed: variable " <> i <> " not bound")
  IfElse _ tst thn els -> do
    (psTst, tTst) <- tiExpr ce as tst
    (psThn, tThn) <- tiExpr ce as thn
    (psEls, tEls) <- tiExpr ce as els
    -- The "test" value of the if-else must be a Bool
    unify tBool tTst

    -- Both arms of the if statement must be the same type.
    t <- newTVar Star
    unify tThn t
    unify tEls t
    return (psTst ++ psThn ++ psEls, tEls)

  -- For lambdas, turn `\x -> e` into `let f x = e in f`
  -- and typecheck that instead
  Lambda ann a x -> do
    n <- newIdNum
    let tmpVar = "$INVALIDVAR" <> show n
    (ps, as') <- tiDefinitions ce as [Definition tmpVar Nothing [Binding [PVar a] x]]
    (qs, t) <- tiExpr ce (as' ++ as) (Var ann tmpVar)
    return (ps ++ qs, t)

  -- Application: the core of a good programming language. First, we
  -- infer the type of both the function and the
  App _ a b -> do
    -- Infer the function (a)
    (ps, ta) <- tiExpr ce as a
    -- Infer the argument (b)
    (qs, tb) <- tiExpr ce as b
    -- Create a new type to represent the type
    -- of the result of this application
    t <- newTVar Star
    -- Unify the type of a (the function) with a new type which
    -- takes b (the arg) and returns some new type.
    unify (tb `fn` t) ta
    -- Then simply return the new t we created
    return (ps ++ qs, t)

  -- Infix syntax is just a magic version of apply
  Inf a op l r -> do
    tiExpr ce as (App a (App a (Var a op) l) r)

  -- A native call is just the type that the user provides. We just trust them on this.
  NativeCall _ _ t _ -> do
    -- if the return type of the native call has variables, we must error.
    if null (tv t)
      then return ([], t) -- There are no vars. All good!
      else throwError (makeExpressionTypeError e "Native calls must not have type variables")

  -- a match satement is basically a series of `Binding` values in fancy clothes.
  -- The key observation is that each of them must have the same type for the pattern,
  -- and the same type for the resulting value
  Match _ e arms -> do
    -- Create a new type to represent the type
    -- of the result of this match expression
    t <- newTVar Star
    -- Infer the type of the matchee (in `match x {}`, x is the matchee)
    (ps, te) <- tiExpr ce as e
    -- infer the types of the arms
    r <- mapM (tiMatchArm ce as) arms
    -- extract the pattern types...
    let pts = map (fst . snd) r
    -- and unify them together and with the matchee
    unifyList (te:pts)
    -- extract the body types...
    let bts = map (snd . snd) r
    -- and unify them with the result variable we made above
    unifyList (t:bts)
    -- extract the predicates
    let ps' = foldl (++) ps (map fst r)
    -- and return them with the unified result type
    return (ps', t)

tiMatchArm :: Infer (MatchArm SourceRange) (Type, Type)
tiMatchArm ce as arm@(MatchArm p e) = do
  (ps, as', tp) <- tiPat p as
  (qs, te) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, (tp, te))

tiBinding :: Infer (Binding SourceRange) Type
tiBinding ce as (Binding pats e) = do
  (ps, as', ts) <- tiPats pats as
  (qs, t) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, foldr fn t ts)

tiBindings :: ClassEnv -> [Assump] -> [Binding SourceRange] -> Type -> TI [Pred]
tiBindings ce as bindings t = do
  psts <- mapM (tiBinding ce as) bindings
  mapM_ (unify t . snd) psts
  return (concatMap fst psts)

--
-- Type inference for pattern expressions - expressions used in pattern matching
--
tiPat :: Pat -> [Assump] -> TI ([Pred], [Assump], Type)
tiPat p as = case p of
  -- Simple variable matching. Not conditional, just a simple binding.
  PVar i -> do
    v <- newTVar Star
    return ([], [i :>: toScheme v], v)

  -- Wildcard matching
  PWildcard -> do
    v <- newTVar Star
    return ([], [], v)

  -- For "as" patterns (a@pat), simply infer the type of the pat
  -- and name that assumption according to the id before the '@'
  PAs id pat -> do
    (ps, as, t) <- tiPat pat as
    return (ps, (id :>: toScheme t) : as, t)

  -- For a constructor pattern
  PCon (i :>: _) pats ->
    -- Lookup the constructor's ID in the assumption list, assuming the constructor
    -- function has an assumption.
    case findAssump i as of
      Nothing -> throwError $ TEGeneric ("Failed: constructor " <> i <> " not defined")
      Just sc -> do
        (qs :=> t) <- freshInst sc
        -- Infer the types of the pattern matched arguments
        (ps, as, ts) <- tiPats pats as
        -- create a new type
        t' <- newTVar Star
        let ft = foldr fn t' ts
        -- create a new instance of the type assumed in the constructor
        -- arg. We then unify this new type with the
        unify t ft
        return (ps ++ qs, as, t')

--
--
--
-- Infer on a list of patterns
tiPats :: [Pat] -> [Assump] -> TI ([Pred], [Assump], [Type])
tiPats pats as = do
  psasts <- mapM (`tiPat` as) pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  return (ps, as, ts)

tiDefinitions :: Infer [Definition SourceRange] [Assump]
tiDefinitions ce as defs = do
  -- Split the definitions into "Explicit" and "Implicit" groups based on
  -- if they are provided with a type or now.
  let es = [Expl v sc alts | o@(Definition v (Just sc) alts) <- defs]
  let iss = [Impl v alts | o@(Definition v Nothing alts) <- defs]
  -- return ([], as)
  let as' = [v :>: sc | (Expl v sc alts) <- es]
  (ps, as'') <- tiImplicitBindings ce (as' ++ as) iss
  qss <- mapM (tiExplicitBinding ce (as'' ++ as' ++ as)) es
  return (ps ++ concat qss, as'' ++ as')

tiExplicitBinding :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExplicitBinding ce as ex@(Expl i sc alts) =
  do
    (qs :=> t) <- freshInst sc
    ps <- tiBindings ce as alts t
    s <- getSubst
    let qs' = apply s qs
        t' = apply s t
        fs = tv (apply s as)
        gs = tv t' \\ fs
        sc' = quantify gs (qs' :=> t')
        ps' = filter (not . entail ce qs') (apply s ps)
    (ds, rs) <- split ce fs gs ps'
    if sc /= sc'
      then liftEither $ genericTypeError ("signature too general:\n\t" <> show ex <> "\n\tsc:" <> show sc <> "\n\tsc':" <> show sc')
      else
        if not (null rs)
          then liftEither $ genericTypeError "context too weak"
          else return ds

restricted :: [Impl] -> Bool
restricted = any simple
  where
    simple (Impl i alts) = any (null . bindingPatterns) alts

tiImplicitBindings :: Infer [Impl] [Assump]
tiImplicitBindings ce as [] = return ([], as)
tiImplicitBindings ce as bs = do
  -- Create a bunch of unnamed types
  ts <- mapM (const $ newTVar Star) bs
  let names = map implName bs -- Grab the names
      scs = map toScheme ts -- Grab the schemes
      -- Create some temp assumptions based on those scheme/name combos
      as' = zipWith (:>:) names scs ++ as
      -- Extract the bindings from the impls
      bindings = map implBindings bs
  -- Run type inference on each binding together, and grab the predicates
  preds <- sequence (zipWith (tiBindings ce as') bindings ts)
  s <- getSubst -- Grab the current substitution list
  let ps' = apply s (concat preds) -- Apply the subst to those predicates
      ts' = apply s ts -- apply the subst to the types we created earlier
      fs = tv (apply s as) -- apply the subst to the assumptions, and grab the free type vars
      vss = map tv ts' -- Gather up all the type vars from each substituted type
      -- Fold Unification for each `vss` that isn't in `fs`
      gs = foldr1 union vss \\ fs
  -- apply the split function to
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restricted bs
    then
      let gs' = gs \\ tv rs
          scs' = map (quantify gs' . ([] :=>)) ts'
       in return (ds ++ rs, zipWith (:>:) names scs')
    else
      let scs' = map (quantify gs . (rs :=>)) ts'
       in return (ds, zipWith (:>:) names scs')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as [] = return ([], [])
tiSeq ti ce as (bs : bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

--------------------------------------------------------------------------------
split ::
  ClassEnv -> -- ce
  [Tyvar] -> -- fixed
  [Tyvar] -> -- toQuant
  [Pred] -> -- ps
  TI ([Pred], [Pred])
split ce fixed toQuant ps = do
  -- ce: the class envirionment
  -- fixed: the set of "fixed" variables. Those which appear free in the assumptions.
  -- toQuant: variables we would like to quantify.
  -- ps: the predicates we want ot operate on.
  --
  -- First, reduce the predicates. Remember that this will, in effect, take a precdicate
  -- `Eq (a, b)` and transform it to `(Eq a, Eq b)`, which is still valid iff `Eq (a, b)`
  -- is valid.
  ps' <- liftEither $ reduce ce ps
  -- Partition the list of predicates into those who's
  -- type variables are all in `fs` and those who don't.
  let (ds, rs) = partition (all (`elem` fixed) . tv) ps'
  -- Now, apply defaults to the side of the partition
  -- without all their values being found in `fs`
  rs' <- defaultedPreds ce (fixed ++ toQuant) rs
  -- And simply return them, but with the difference between
  -- rs and rs' being applied
  return (ds, rs \\ rs')

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  -- Given an Ambiguity, look through the class envirionment and
  -- return the types it *could* refer to.
  --
  -- If this function returns an empty list,
  [ t'
    | -- Extract two lists from the predicates, one for the class names
      -- and one for the type names which are predicated on those classes
      let is = [i | IsIn i t <- qs]
          ts = [t | IsIn i t <- qs],
      all (TVar v ==) ts,
      -- currently, we only know how to resolve ambiguities with the standard
      -- classes, so we only care about those.
      any (`elem` numClasses) is,
      all (`elem` stdClasses) is,
      -- For each t' in the defaults...
      t' <- defaults ce,
      -- it must pass an entail check for each `i` in `is`
      all (entail ce []) [IsIn i t' | i <- is]
  ]

-- TODO: remove me when I support this syntax.
stdClasses :: [Id]
stdClasses =
  [ "Eq",
    "Ord",
    "Show",
    "Read",
    "Bounded",
    "Enum",
    "Ix",
    "Functor",
    "Monad",
    "MonadPlus"
  ]
    ++ numClasses

type Ambiguity = (Tyvar, [Pred])

ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]

numClasses :: [Id]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

withDefaults ::
  ([Ambiguity] -> [Type] -> a) ->
  ClassEnv ->
  [Tyvar] ->
  [Pred] ->
  TI a
withDefaults f ce vs ps
  | any null tss = throwError (TEGeneric ("cannot resolve ambiguity. vars:" <> show vps <> " ps: " <> show ps))
  | otherwise = return (f vps (map head tss))
  where
    vps = ambiguities ce vs ps
    tss = map (candidates ce) vps

defaultedPreds :: ClassEnv -> [Tyvar] -> [Pred] -> TI [Pred]
defaultedPreds = withDefaults (\vps ts -> concatMap snd vps)

defaultSubst :: ClassEnv -> [Tyvar] -> [Pred] -> TI Subst
defaultSubst ce ts ps = withDefaults (zip . map fst) ce ts ps

tiProgram :: ClassEnv -> [Assump] -> [Program SourceRange] -> Either TypeError [Assump]
tiProgram ce as progs = runTI $
  do
    (ps, as') <- tiSeq tiDefinitions ce as (map defs progs)
    s <- getSubst
    rs <- liftEither $ reduce ce (apply s ps)
    s' <- defaultSubst ce [] rs
    return $ fromAssumpMap (toAssumpMap (apply (s' @@ s) as'))
