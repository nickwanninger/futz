{-# LANGUAGE LambdaCase #-}

module Futz.Lexer.Layout where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Futz.Syntax

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
infixl 1 |>

-- A forward composition operator, that I find prettier than (&)
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

infixr 0 <|

-- A backwards composition operator, that I find prettier than ($)
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

infixr 9 <<<

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
g <<< f = g . f
{-# INLINE (<<<) #-}

infixl 9 >>>

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}

-- Map over a list monadically, then squash the results monoidally
foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f = mapM f >>> fmap mconcat

-- Transform an either by mapping on its left side
mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = either (f >>> Left) Right

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------

-- A layout is either one explicitly declared by the user, or implicitly declared at a certain column
data Layout = Explicit | Implicit Int
  deriving (Eq, Show)

-- Represents the state we have access to as we're laying out our tokens
--
-- We have a current stack of layouts, a stream of tokens, and a flag to know if the
-- we're looking to start a layout with the next token.
data LayoutState = LayoutState
  { layouts :: [Layout],
    tokens :: [Token],
    expectingLayout :: Bool
  }

-- The Monadic context we use for laying out tokens.
--
-- We might fail with an error, and otherwise we have access to a context we can modify.
type LayoutM a = ExceptT String (State LayoutState) a

-- Produce a token
yieldToken :: Token -> LayoutM ()
yieldToken t = modify' (\s -> s {tokens = t : tokens s})

-- Push a new layout onto our stack
pushLayout :: Layout -> LayoutM ()
pushLayout l = modify' (\s -> s {layouts = l : layouts s})

-- Pop a layout from our stack.
--
-- This has no effect if our stack is empty.
popLayout :: LayoutM ()
popLayout = modify' (\s -> s {layouts = drop 1 (layouts s)})

-- Get the current layout, if it exists.
currentLayout :: LayoutM (Maybe Layout)
currentLayout = gets layouts |> fmap listToMaybe

-- Run the layout context, producing either an error, or the tokens with the inferred layout tokens.
runLayoutM :: LayoutM a -> Either String [Token]
runLayoutM =
  runExceptT >>> (`runState` LayoutState [] [] False) >>> \case
    (Left e, _) -> Left e
    (Right _, LayoutState _ ts _) -> Right (reverse ts)

-- Compare a level of indentation with the current layout.
--
-- The provided column is greater than no layout, or an explicit layout. And
-- compares with an implicit layout based on its column.
compareIndentation :: Int -> LayoutM Ordering
compareIndentation col =
  let cmp Nothing = GT
      cmp (Just Explicit) = GT
      cmp (Just (Implicit n)) = compare col n
   in fmap cmp currentLayout

-- Take a stream of positioned tokens, and produce either an error, or the tokens
-- with semicolons and braces inserted judiciously.
layout :: [Token] -> Either String [Token]
layout inputs =
  runLayoutM <| do
    mapM_ step inputs
    closeImplicitLayouts
  where
    startsLayout :: Lexeme -> Bool
    startsLayout LStartLayout = True
    startsLayout _ = False

    endsLayout :: Lexeme -> Bool
    endsLayout LEndLayout = True
    endsLayout _ = False

    step :: Token -> LayoutM ()
    step tok@(Tok pos@(Pos line col) l s) = do
      expectingLayout' <- gets expectingLayout
      case l of
        LClose -> closeExplicitLayout
        LOpen | expectingLayout' -> startExplicitLayout
        _
          | startsLayout l -> do
              modify' (\s -> s {expectingLayout = True})
              continueImplicitLayout col
          | expectingLayout' -> startImplicitLayout col
          | otherwise -> continueImplicitLayout col
      -- \| col == 0 -> continueImplicitLayout col
      -- \| otherwise -> return ()
      yieldToken tok

    closeExplicitLayout :: LayoutM ()
    closeExplicitLayout =
      currentLayout >>= \case
        Just Explicit -> popLayout
        _ -> throwError "Unexpected }"

    startExplicitLayout :: LayoutM ()
    startExplicitLayout = do
      modify' (\s -> s {expectingLayout = False})
      pushLayout Explicit

    startImplicitLayout :: Int -> LayoutM ()
    startImplicitLayout col = do
      modify' (\s -> s {expectingLayout = False})
      -- Regardless of what happens, we're starting a layout...
      compareIndentation col >>= \case
        GT -> do
          yieldToken $ VTok LOpen
          pushLayout (Implicit col)
        -- But if we're not indented further, we're immediately ending that layout.
        -- Furthermore, we might be continuing an implicit layout.
        _ -> do
          yieldToken $ VTok LOpen
          yieldToken $ VTok LClose
          continueImplicitLayout col

    continueImplicitLayout :: Int -> LayoutM ()
    continueImplicitLayout col = do
      closeFurtherLayouts
      compareIndentation col >>= \case
        EQ -> yieldToken $ VTok LSemi
        _ -> return ()
      where
        closeFurtherLayouts =
          compareIndentation col >>= \case
            LT -> do
              yieldToken $ VTok LClose
              popLayout
              closeFurtherLayouts
            _ -> return ()

    closeImplicitLayouts :: LayoutM ()
    closeImplicitLayouts =
      currentLayout >>= \case
        Nothing -> return ()
        Just Explicit -> throwError "Unmatched Layout"
        Just (Implicit _) -> do
          yieldToken $ VTok LClose
          popLayout
          closeImplicitLayouts

-- applyLayout :: [Token] -> Either LexerError [Token]
-- applyLayout ts = layout ts

-- -- The main public interface
-- applyLayout :: [Token] -> [Token]
-- applyLayout [] = []
-- applyLayout (t : ts) = case t of
--   -- If we find a "layout" token, enter the layout state
--   Tok pos@(Pos line col) LLayout str -> t' : (applyLayout ts)
--     where
--       t' = Tok pos LSyntax str

--   -- Handle newlines
--   Tok pos@(Pos line col) LNewline _ -> applyLayout ts
--   -- For everything else
--   _ -> t : applyLayout ts
