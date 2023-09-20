-- The type system described here is basically
-- line-for-line what is in "Typing Haskell in Haskell"
{-# LANGUAGE InstanceSigs #-}

module Futz.Types where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Either.Combinators
import Data.Foldable ()
import Data.List (intercalate, intersect, nub, union)
import qualified Data.Map as Map
import Data.Maybe

type TVar = String

type Id = String

enumId :: Int -> Id
enumId n = "v" ++ show n

data Kind = Star | Kfun Kind Kind
  deriving (Eq, Ord)

instance Show Kind where
  show Star = "*"
  -- TODO: pretty!
  show (Kfun a b) = "(" <> show a <> " -> " <> show b <> ")"

data Type
  = TVar Tyvar
  | TCon Tycon
  | TAp Type Type -- A type application
  | TGen Int
  deriving (Eq, Ord)

data Tyvar = Tyvar Id Kind
  deriving (Eq, Ord)

-- Convenient construction function for tyvar
tyvar :: Id -> Tyvar
tyvar id = Tyvar id Star

data Tycon = Tycon Id Kind
  deriving (Eq, Ord)

instance Show Type where
  show (TVar name) = show name
  show (TCon name) = show name
  -- Arrow is a little interesting to print. For clairty we put
  -- parens around the argument if it itself is an Arrow, to be clear
  show ap@(TAp a@(TAp t a') b)
    | t /= tArrow = plainShowAp a b
    | isArrowType a' = "(" <> show a' <> ") -> " <> show b
    | otherwise = show a' <> " -> " <> show b
  show (TAp a b) = plainShowAp a b
  -- Unsure how to show this. I'll just show them as invalid syntax for now.
  show (TGen i) = greekify i
    where
      greek = "αβγδεζηθικλμνξοπρστυφχψω"
      greekify i = case greek ^? element i of
        Just g -> [g]
        Nothing -> greekify (i `mod` 24) ++ show (i `div` 24)

plainShowAp a b = "(" <> show a <> " " <> show b <> ")"

isArrowType :: Type -> Bool
isArrowType (TAp a@(TAp t a') b) = t == tArrow
isArrowType _ = False

instance Show Tyvar where
  show (Tyvar id _) = id

instance Show Tycon where
  show (Tycon id _) = id

tUnit = TCon (Tycon "()" Star)

tChar = TCon (Tycon "Char" Star)

tInt = TCon (Tycon "Int" Star)

-- tInteger = TCon (Tycon "Integer" Star)
tBool = TCon (Tycon "Bool" Star)

tFloat = TCon (Tycon "Float" Star)

tDouble = TCon (Tycon "Double" Star)

tList = TCon (Tycon "[]" (Kfun Star Star))

tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))

tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

infixr 4 `fn`

fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair a = TAp (TAp tTuple2 a)

-- given a type name, and a list of types, produce a TAp w/ a fold
fromDataDecl :: Id -> [Id] -> Type
fromDataDecl name = foldl (\t a -> TAp t (TVar (tyvar a))) (TCon (Tycon name Star))

-- TAp (TAp (TCon Maybe) (TVar a)) (TVar b)

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar v k) = k

instance HasKind Tycon where
  kind (Tycon v k) = k

instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u) = kind u
  kind (TAp t _) = case kind t of
    (Kfun _ k) -> k
    k -> k

data TypeError
  = TEGeneric String -- A generic type error. Avoid using this.
  | TypeMismatch Type Type
  | MergeFailure Subst Subst
  | UnificationFailure Type Type
  | KindMismatch Type Type
  deriving (Eq)

instance Show TypeError where
  show (TEGeneric s) = s
  show (TypeMismatch t1 t2) =
    unlines
      [ "Type mismatch: " <> show t1,
        "          and: " <> show t2
      ]
  show (MergeFailure s1 s2) = "Merge failure between " <> show s1 <> " and " <> show s2
  show (UnificationFailure t1 t2) =
    unlines
      [ "[ERROR] Could not unify: " <> show t1,
        "                    and: " <> show t2
      ]
  show (KindMismatch a b) =
    unlines
      [ "Kinds do not match between: " <> show a <> " of kind " <> show (kind a),
        "                       and: " <> show b <> " of kind " <> show (kind b)
      ]

genericTypeError :: String -> Either TypeError a
genericTypeError = throwError . TEGeneric

-- a substitution from one tyvar to another type, where they both have the same Kind
type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t =
  if kind u == kind t -- Validate that this substitution is kind-preserving
    then [(u, t)]
    else undefined

class Types t where
  -- apply a substition to a t, returning a new t
  apply :: Subst -> t -> t

  -- given a t, return the type type variables in that t
  tv :: t -> [Tyvar]

instance Types Type where
  apply s (TVar u) = case lookup u s of
    Just t -> t
    Nothing -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  -- everything else is a NOP
  apply s t = t

  tv (TVar u) = [u]
  tv (TAp l r) = tv l `union` tv r
  tv t = [] -- Nothing else has type variables

instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv

-- The apply function can be used to build more complex substitutions.
-- For example, composition of substitutions, satisfying
--      apply (s1 @@ s2) = apply s1 . apply s2
-- can be defined using:
infixr 4 @@

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

-- We can also form a “parallel” composition s1++s2 of two substitutions
-- s1 and s2, but the result is left-biased because bindings in s1 take
-- precedence over any bindings for the same variables in s2. For a more
-- symmetric version of this operation, we use a merge function, which
-- checks that the two substitutions agree at every variable in the domain
-- of both and hence guarantees that apply (s1++s2) = apply (s2++s1).
-- Clearly, this is a partial function, which we reflect by arranging
-- for merge to return its result in a monad, using the standard fail
-- function to provide a string diagnostic in cases where the function
-- is undefined.
merge :: Subst -> Subst -> Either TypeError Subst
merge s1 s2 = if agree then return (s1 ++ s2) else throwError (MergeFailure s1 s2)
  where
    agree =
      all
        (\v -> apply s1 (TVar v) == apply s2 (TVar v))
        (map fst s1 `intersect` map fst s2)

-- mgu: most general unifier
mgu :: Type -> Type -> Either TypeError Subst
mgu (TAp l r) (TAp l' r') = do
  -- Unify both the left and right, then apply the substitutions
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu a@(TCon tc1) b@(TCon tc2)
  | tc1 == tc2 = return nullSubst
  | otherwise = throwError (UnificationFailure a b)
mgu a b = throwError (UnificationFailure a b)

varBind :: MonadError TypeError m => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u = return nullSubst
  -- \| u `elem` tv t = fail "occurs check fails"
  | kind u /= kind t =
      throwError (KindMismatch (TVar u) t)
  | otherwise = return (u +-> t)

-- matching is closely related to unification. Given two types t1, t2, the goal is
-- to find a substitution `s` such that `apply s t1 == t2`. Because the substitution
-- is applied to only one type, this operation is often described as one-way matching.
match :: Type -> Type -> Either TypeError Subst
match (TAp l r) (TAp l' r') = do
  sl <- match l l'
  sr <- match r r'
  merge sl sr
match (TVar u) t | kind u == kind t = return (u +-> t)
match (TCon tc1) (TCon tc2)
  | tc1 == tc2 = return nullSubst
match t1 t2 = throwError (TypeMismatch t1 t2)

-- Qualified types and Type Classes. This is a type `t` where several predicates
-- exist to qualify the type to whatever degree is needed.
data Qual t = [Pred] :=> t
  deriving (Eq, Ord)

instance HasKind (Qual Type) where
  kind (ps :=> t) = kind t

-- It would be easy to extend the Pred datatype to
-- allow other forms of predicate, as is done with
-- Trex records in Hugs [Jones & Peterson, 1999].
-- Another frequently requested extension is to
-- allow classes to accept multiple parameters,
-- which would require a list of Types rather than
-- the single Type in the definition below.
--
-- A predicate indicates that a type must follow some rule.
-- IsIn, for example, says that the type must be an instance
-- of the class Id
data Pred = IsIn Id Type
  deriving (Eq, Ord)

instance Show t => Show (Qual t) where
  show ([] :=> t) = show t
  show (ps :=> t) = unwords (map show ps) <> " => " <> show t

instance Show Pred where
  show (IsIn id t) = "(" <> id <> " " <> show t <> ")"

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn i t) = tv t

mguPred, matchPred :: Pred -> Pred -> Either TypeError Subst
mguPred = liftPred mgu
matchPred = liftPred match

liftPred :: MonadError TypeError m => (Type -> Type -> m a) -> Pred -> Pred -> m a
liftPred m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = throwError (TEGeneric "classes differ")

---- Type Class and Instances
-- we will represent Class declarations as a class name,
-- a list of super classes, and a list of instances
data Class = Class Id [Id] [Inst]
  deriving (Eq, Ord, Show)

type Inst = Qual Pred

-- A simplified version of the standard Haskell class Ord might be
-- described by the following value of type Class:
--
--     (["Eq"], [[] :=> IsIn "Ord" tUnit,
--               [] :=> IsIn "Ord" tChar,
--               [] :=> IsIn "Ord" tInt,
--               [IsIn "Ord" (TVar (Tyvar "a" Star)),
--                IsIn "Ord" (TVar (Tyvar "b" Star))]
--                  :=> IsIn "Ord" (pair (TVar (Tyvar "a" Star))
--                                       (TVar (Tyvar "b" Star)))])
--
-- This structure captures the fact that Eq is a superclass of Ord
-- (the only one in fact), and lists four instance declarations for
-- the unit, character, integer, and pair types (if a and b are in
-- Ord, then (a,b) is also in Ord). Of course, this is only a fraction
-- of the list of Ord instances that are defined in the full Haskell
-- prelude. Only the details that are needed for type inference are
-- included in these representations. A full Haskell implementation
-- would need to store additional information for each declaration,
-- such as the list of member functions for each class and details
-- of their implementations in each particular instance.

--   Class Environments
-- The information provided by the class and instance declarations
-- in a given program can be caputured by the class environment:
data ClassEnv = ClassEnv
  { classMap :: Map.Map Id Class,
    defaults :: [Type]
  }
  deriving (Show)

classes :: ClassEnv -> Id -> Maybe Class
classes ce i = Map.lookup i (classMap ce)

-- The classes field of ClassEnv is a lambda which returns the class
-- for a given ID if it matches. See `modifyClassEnv` below for how that works.
-- The defaults field of ClassEnv is a list of default types.
-- (TODO: what is that?)

-- super: get a list of superclasses from a class, `id`
-- (Does not check that the class exists!)
super :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (Class _ is its) -> is

-- insts: get a list of instances from a class, `id`
-- (Does not check that the class exists!)
insts :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of
  Just (Class _ is its) -> its
  Nothing -> []

-- These functions are intended to be used only in cases where it
-- is known that the class i is defined in the environment ce. In
-- some cases, this condition might be guaranteed by static analysis
-- prior to type checking. Alternatively, we can resort to a dynamic
-- check by testing defined (classes ce i) before applying either
-- function. The function defined used here is defined as follows:
defined :: Maybe a -> Bool
defined (Just x) = True
defined Nothing = False

-- We will also define a helper function, modifyClassEnv, to describe how
-- a class environment can be updated to reflect a new binding of
-- a Class value to a given identifier:
modifyClassEnv :: ClassEnv -> Id -> Class -> ClassEnv
modifyClassEnv ce i c =
  ce
    { classMap = Map.insert i c (classMap ce)
    }

initialEnv :: ClassEnv
initialEnv =
  ClassEnv
    { classMap = Map.empty,
      defaults = [tInt, tDouble]
    }

-- As we process each class or instance declaration in a program,
-- we transform the initial class environment to add entries, either
-- for a new class, or for a new instance, respectively. In either case,
-- there is a possibility that the new declaration might be incompatible
-- with the previous declarations, attempting, for example, to redefine
-- an existing class or instance. For this reason, we will describe
-- transformations of a class environment as functions of the EnvTransformer
-- type, using a Maybe type to allow for the possibility of errors:
type EnvTransformer m = ClassEnv -> m ClassEnv

-- The sequencing of multiple transformers can be described by a (forward)
-- composition operator (<:>):
infixr 5 <:>

(<:>) :: MonadError TypeError m => EnvTransformer m -> EnvTransformer m -> EnvTransformer m
(f <:> g) ce = do
  ce' <- f ce
  g ce'

-- To add a new class to an environment, we must check that there is not
-- already a class with the same name, and that all of the named superclasses
-- are already defined. This is a simple way of enforcing Haskell's restriction
-- that the superclass hierarchy be acyclic. Of course, in practice, it will be
-- necessary to topologically sort the set of class declarations in a program
-- to determine a suitable ordering; any cycles in the hierarchy will typically
-- be detected at this stage.
addClass :: Id -> [Id] -> ClassEnv -> Either TypeError ClassEnv
addClass i is ce
  | defined (classes ce i) = genericTypeError ("class " <> i <> " is already defined")
  | not (all (defined . classes ce) is) = genericTypeError ("superclass not defined (" <> i <> ")")
  | otherwise = return (modifyClassEnv ce i (Class i is []))

-- For example, we can describe the effect of the class declarations in the
-- Haskell prelude using the following transformer:
-- TODO: REMOVE THESE WHEN YOU BUILD IT INTO THE SYNTAX

addPreludeClasses :: ClassEnv -> Either TypeError ClassEnv
addPreludeClasses = addCoreClasses <:> addNumClasses

-- This definition breaks down the set of standard Haskell classes into
-- two separate pieces. The core classes are described as follows:
addCoreClasses :: ClassEnv -> Either TypeError ClassEnv
addCoreClasses =
  addClass "Eq" []
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" []
    <:> addClass "Read" []
    <:> addClass "Bounded" []
    <:> addClass "Enum" []
    <:> addClass "Functor" []
    <:> addClass "Monad" []

-- The hierarchy of numeric classes is captured separately in
-- the following definition:
addNumClasses :: ClassEnv -> Either TypeError ClassEnv
addNumClasses =
  addClass "Num" ["Eq", "Show"]
    <:> addClass "Real" ["Num", "Ord"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass "Integral" ["Real", "Enum"]
    <:> addClass "RealFrac" ["Real", "Fractional"]
    <:> addClass "Floating" ["Fractional"]
    <:> addClass "RealFloat" ["RealFrac", "Floating"]

-- To add a new instance to a class, we must check that the class to
-- which the instance applies is defined, and that the new instance
-- does not overlap with any previously declared instance:
addInst :: [Pred] -> Pred -> ClassEnv -> Either TypeError ClassEnv
addInst ps p@(IsIn className _) ce
  | not (defined (classes ce className)) = genericTypeError ("no class for instance " <> className)
  | any (overlap p) qs = genericTypeError "overlapping instance"
  | otherwise = return (modifyClassEnv ce className c)
  where
    instances = insts ce className
    supes = super ce className
    qs = [q | (_ :=> q) <- instances]
    c = Class className supes ((ps :=> p) : instances)

-- Two instances for a class are said to overlap if there is some
-- predicate that is a substitution instance of the heads of both
-- instance declarations. It is easy to test for overlapping predicates
-- using the functions that we have defined previously:
--
-- This test covers simple cases where a program provides two instance
-- declarations for the same type (for example, two declarations for Eq Int),
-- but it also covers cases where more interesting overlaps occur (for example,
-- between the predicates Eq [Int] and Eq [a], or between predicates Eq (a,Bool)
-- and Eq (Int,b)). In each case, the existence of an overlap indicates the
-- possibility of a semantic ambiguity, with two applicable instance declarations,
-- and no clear reason to prefer one over the other. This is why Haskell treats
-- such overlaps as an error. Extensions to Haskell to support overlapping instances
-- in certain special cases have been considered elsewhere; they appear to have
-- interesting applications, but also have some potentially troublesome impact
-- on program semantics [Peyton Jones et al. , 1997]. We will not consider such
-- issues further here.
overlap :: Pred -> Pred -> Bool
overlap p q = case mguPred p q of
  Left _ -> False
  Right r -> defined (Just r)

-- To illustrate how the addInst function might be used, the following definition
-- shows how the standard prelude class environment can be extended to include the
-- four instances for Ord from the example in Basic Definitions.
exampleInsts :: ClassEnv -> Either TypeError ClassEnv
exampleInsts =
  addPreludeClasses
    <:> addInst [] (IsIn "Show" tInt)
    <:> addInst [] (IsIn "Num" tInt)
    <:> addInst [] (IsIn "Num" tDouble)
    <:> addInst [] (IsIn "Ord" tUnit)
    <:> addInst [] (IsIn "Ord" tChar)
    <:> addInst [] (IsIn "Ord" tInt)
    <:> addInst
      [ IsIn "Ord" (TVar (Tyvar "a" Star)),
        IsIn "Ord" (TVar (Tyvar "b" Star))
      ]
      ( IsIn
          "Ord"
          ( pair
              (TVar (Tyvar "a" Star))
              (TVar (Tyvar "b" Star))
          )
      )

--
--  Entailment
--
-- In this section, we describe how class environments can be used to answer
-- questions about which types are instances of particular classes. More generally,
-- we consider the treatment of entailment:
-- given a predicate p and a list of predicates ps, our goal is to determine whether
-- p will hold whenever all of the predicates in ps are satisfied. In the special case
-- where p = IsIn i t and ps = [], this amounts to determining whether t is an instance
-- of the class i. In the theory of qualified types [Jones, 1992], assertions like this
-- are captured using judgements of the form ps ||- p; we use a different notation
-- here-the entail function that is defined at the end of this section-to make the dependence
-- on a class environment explicit.

-- As a first step, we can ask how information about superclasses and instances can be
-- used independently to help reason about entailments. For example, if a type is an
-- instance of a class i, then it must also be an instance of any superclasses of i.
-- Hence, using only superclass information, we can be sure that, if a given predicate
-- p holds, then so too must all of the predicates in the list bySuper p:
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
  p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]

-- The list bySuper ce p may contain duplicates, but it will always be finite because
-- of the restriction that the superclass hierarchy is acyclic.
--
-- Next we consider how information about instances can be used. Of course, for a given
-- predicate p = IsIn i t, we can find all the directly relevant instances in a class
-- environment ce by looking in insts ce i. As we have seen, individual instance declarations
-- are mapped into clauses of the form ps :=> h. The head predicate h describes the general
-- form of instances that can be constructed from this declaration, and we can use matchPred
-- to determine whether this instance is applicable to the given predicate p. If it is applicable,
-- then matching will return a substitution u, and the remaining subgoals are the elements of
-- map (apply u) ps. The following function uses these ideas to determine the list of subgoals
-- for a given predicate:
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [rightToMaybe (tryInst it) | it <- insts ce i]
  where
    tryInst (ps :=> h) = do
      u <- matchPred h p
      Right (map (apply u) ps)

-- The msum function used here comes from the standard Monad library, and returns the first
-- defined element in a list of Maybe values; if there are no defined elements in the list,
-- then it returns Nothing. Because Haskell prevents overlapping instances, there is at most
-- one applicable instance for any given p, and we can be sure that the first defined element
-- will actually be the only defined element in this list.
--
-- The bySuper and byInst functions can be used in combination to define a general entailment
-- operator, entail. Given a particular class environment ce, the intention here is that entail
-- ce ps p will be True if, and only if, the predicate p will hold whenever all of the predicates
-- in ps are satisfied:

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p =
  any ((p `elem`) . bySuper ce) ps
    || case byInst ce p of
      Nothing -> False
      Just qs -> all (entail ce ps) qs

-- The first step here is to determine whether p can be deduced from ps using only superclasses.
-- If that fails, we look for a matching instance and generate a list of predicates qs as a new
-- goal, each of which must, in turn, follow from ps.
--
-- Conditions specified in the Haskell report-namely that the class hierarchy is acyclic and that
-- the types in any instance declaration are strictly smaller than those in the head-translate into
-- conditions on the values for the ClassEnv that can be passed in as ce, and these are enough to
-- guarantee that tests for entailment will terminate. Completeness of the algorithm is also
-- important: will entail ce ps p always return True whenever there is a way to prove p from ps?
-- In fact our algorithm does not cover all possible cases: it does not test to see if p is a
-- superclass of some other predicate q for which entail ce ps q is True. Extending the algorithm
-- to test for this would be very difficult because there is no obvious way to choose a particular
-- q, and, in general, there will be infinitely many potential candidates to consider. Fortunately,
-- a technical condition in the Haskell report [Peyton Jones & Hughes, 1999,Condition 1 on Page 47]
-- reassures us that this is not necessary: if p can be obtained as an immediate superclass of some
-- predicate q that was built using an instance declaration in an entailment `entail ce ps q`, then
-- ps must already be strong enough to deduce p. Thus, although we have not formally proved these
-- properties, we believe that our algorithm is sound, complete, and guaranteed to terminate.

--
--   Context Reduction
--
-- Class environments also play an important role in an aspect of the Haskell type system that is
-- known as context reduction. The basic goal of context reduction is to reduce a list of predicates
-- to an equivalent but, in some sense, simpler list. The Haskell report [Peyton Jones & Hughes, 1999]
-- provides only informal hints about this aspect of the Haskell typing, where both pragmatics and
-- theory have important parts to play. We believe therefore that this is one of the areas where a
-- more formal specification will be particularly valuable.
--
-- One way to simplify a list of predicates is to simplify the type components of individual predicates
-- in the list. For example, given the instance declarations in the Haskell standard prelude, we could
-- replace any occurrences of predicates like Eq [a], Eq (a,a), or Eq ([a],Int) with Eq a. This is valid
-- because, for any choice of a, each one of these predicates holds if, and only if, Eq a holds. Notice
-- that, in some cases, an attempt to simplify type components-for example, by replacing Eq (a, b) with
-- (Eq a, Eq b)-may increase the number of predicates in the list. The extent to which simplifications
-- like this are used in a system of qualified types has an impact on the implementation and performance
-- of overloading in practical systems [Jones, 1992,Chapter 7]. In Haskell, however, the decisions are
-- made for us by a syntactic restriction that forces us to simplify predicates until we obtain types
-- in a kind of “head-normal form”. This terminology is motivated by similarities with the concept of
-- head-normal forms in l-calculus. More precisely, the syntax of Haskell requires class arguments to
-- be of the form v t1 ... tn, where v is a type variable, and t1,...,tn are types (and n ³ 0). The
-- following function allows us to determine whether a given predicate meets these restrictions:
inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
  where
    hnf (TVar v) = True
    hnf (TCon tc) = False
    hnf (TAp t _) = hnf t

-- Predicates that do not fit this pattern must be broken down using byInst. In some cases, this will
-- result in predicates being eliminated altogether. In others, where byInst fails, it will indicate
-- that a predicate is unsatisfiable, and will trigger an error diagnostic. This process is captured
-- in the following definition:
toHnfs :: ClassEnv -> [Pred] -> Either TypeError [Pred]
toHnfs ce ps = do
  pss <- mapM (toHnf ce) ps
  return (concat pss)

toHnf :: ClassEnv -> Pred -> Either TypeError [Pred]
toHnf ce p
  | inHnf p = return [p]
  | otherwise = case byInst ce p of
      Nothing -> genericTypeError ("context reduction on " <> show p)
      Just ps -> toHnfs ce ps

-- Another way to simplify a list of predicates is to reduce the number of elements that it contains.
-- There are several ways that this might be achieved: by removing duplicates (e.g., reducing (Eq a, Eq a) to Eq a);
-- by eliminating predicates that are already known to hold (e.g., removing any occurrences of Num Int); or
-- by using superclass information (e.g., reducing (Eq a, Ord a) to Ord a). In each case, the reduced list
-- of predicates, is equivalent to the initial list, meaning that all the predicates in the first will be
-- satisfied if, and only if, all of the predicates in the second are satisfied. The simplification algorithm
-- that we will use here is based on the observation that a predicate p in a list of predicates (p:ps) can
-- be eliminated if p is entailed by ps. As a special case, this will eliminate duplicated predicates: if
-- p is repeated in ps, then it will also be entailed by ps. These ideas are used in the following definition
-- of the simplify function, which loops through each predicate in the list and uses an accumulating parameter
-- to build up the final result. Each time we encounter a predicate that is entailed by the others, we remove
-- it from the list.
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
    loop rs [] = rs
    loop rs (p : ps)
      | entail ce (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps

-- Now we can describe the particular form of context reduction used in Haskell as a combination of
-- toHnfs and simplify. Specifically, we use toHnfs to reduce the list of predicates to head-normal
-- form, and then simplify the result:
reduce :: ClassEnv -> [Pred] -> Either TypeError [Pred]
reduce ce ps = do
  qs <- toHnfs ce ps
  return (simplify ce qs)

-- As a technical aside, we note that there is some redundancy in the definition of reduce. The
-- simplify function is defined in terms of entail, which makes use of the information provided
-- by both superclass and instance declarations. The predicates in qs, however, are guaranteed
-- to be in head-normal form, and hence will not match instance declarations that satisfy the
-- syntactic restrictions of Haskell. It follows that we could make do with a version of simplify
-- that used only the following function in determining (superclass) entailments:
scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any ((p `elem`) . bySuper ce) ps

-------------------------------------------------------------------------------
--
--
--
--
--   Type Schemes
--
-- Type schemes are used to describe polymorphic types, and are represented using a list of
-- kinds and a qualified type:
data Scheme = Forall [Kind] (Qual Type)
  deriving (Eq)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall ks t) = "∀{" <> intercalate "," (map show ks) <> "} " <> show t

-- There is no direct equivalent of Forall in the syntax of Haskell. Instead, implicit
-- quantifiers are inserted as necessary to bind free type variables.
--
-- In a type scheme Forall ks qt, each type of the form TGen n that appears in the qualified
-- type qt represents a generic, or universally quantified type variable whose kind is given
-- by ks!!n. This is the only place where we will allow TGen values to appear in a type. We
-- had originally hoped that this restriction could be captured statically by a careful choice
-- of the representation for types and type schemes. Unfortunately, we have not yet found a
-- satisfactory way to enforce this, and, after considering several alternatives, we have
-- settled for the representation shown here because it allows for simple implementations of
-- equality and substitution. For example, the implementation of apply on Type values ignores
-- TGen values, so we can be sure that there will be no variable capture problems in the
-- following definition:
instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall ks qt) = tv qt

-- Type schemes are constructed by quantifying a qualified type qt with respect
-- to a list of type variables vs:
quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [v | v <- tv qt, v `elem` vs]
    ks = map kind vs'
    s = zip vs' (map TGen [0 ..])

-- Note that the order of the kinds in ks is determined by the order in which the variables v
-- appear in tv qt, and not by the order in which they appear in vs. So, for example, the
-- leftmost quantified variable in a type scheme will always be represented by TGen 0. By
-- insisting that type schemes are constructed in this way, we obtain a unique canonical
-- form for Scheme values. This is important because it means that we can test whether two
-- type schemes are the same. For example, to determine whether an inferred type agrees
-- with a declared type-using Haskell's derived equality, and without having to implement
-- more complex tests for A-equivalence. [https://en.wikipedia.org/wiki/A-equivalence]
--
-- In practice, we sometimes need to convert a Type into a Scheme without adding any qualifying
-- predicates or quantified variables. For this special case, we can use the following function
-- instead of quantify:
toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

-- To complete our description of type schemes, we need to be able to instantiate the
-- quantified variables in Scheme values. In fact, for the purposes of type inference,
-- we only need the special case that instantiates a type scheme with fresh type variables.
-- We therefore defer further description of instantiation to A Type Inference Monad where
-- the mechanisms for generating fresh type variables are introduced.
-------------------------------------------------------------------------------
--
--
--
--
--   Assumptions
--
-- Assumptions about the type of a variable are represented by values of the Assump datatype,
-- each of which pairs a variable with a type scheme
data Assump = Id :>: Scheme
  deriving (Eq)

type AssumpMap = Map.Map Id Assump

toAssumpMap :: [Assump] -> AssumpMap
toAssumpMap = foldl (\m a@(i :>: s) -> Map.insert i a m) Map.empty

fromAssumpMap :: AssumpMap -> [Assump]
fromAssumpMap = Map.elems

instance Show Assump where
  show (id :>: sc) = id <> " :: " <> show sc

-- Once again, we can extend the Types class to allow the application of a
-- substitution to an assumption:
instance Types Assump where
  apply :: Subst -> Assump -> Assump
  apply s (i :>: sc) = i :>: apply s sc
  tv (i :>: sc) = tv sc

-- Thanks to the instance definition for Types on lists (Substitutions), we can also use
-- the apply and tv operators on the lists of assumptions that are used to record the
-- type of each program variable during type inference. We will also use the following
-- function to find the type of a particular variable in a given set of assumptions:
findAssump :: Id -> [Assump] -> Maybe Scheme
findAssump i [] = Nothing
findAssump i ((i' :>: sc) : as)
  | i == i' = Just sc
  | otherwise = findAssump i as

-- This definition allows for the possibility that the variable i might not appear in as.
-- In practice, occurrences of unbound variables will probably have been detected in earlier
-- compiler passes.
