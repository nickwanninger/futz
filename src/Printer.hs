module Printer (
  format
) where


import qualified Parser as P
import qualified Syntax as S
-- import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Writer



newtype FormatState = FMState { indentation :: Int }

-- The Format Monad is an accumulation of strings, that get concatenated
-- along with a state
type FormatM a = WriterT [String] (State FormatState) a

format :: S.Exp -> String
format e = case runState (runWriterT (fmtExp e)) FMState {indentation = 0} of
  ((_, xs), _) -> concat xs


emit :: String -> FormatM ()
emit s = do tell [s]

-- insert a newline, and increase indentation
newline :: Int -> FormatM ()
newline ind = do
  emit "\n"
  new_ind <- indent ind
  replicateM_ new_ind (emit " ")

indent :: Int -> FormatM Int
indent ind = do
  ps <- get
  let new_ind = indentation ps + ind
  put $ FMState { indentation = new_ind }
  return new_ind


fmtBinary :: String -> S.Exp -> S.Exp -> FormatM ()
fmtBinary op l r = do
  emit "("
  fmtExp l
  emit $ " " ++ op ++ " "
  fmtExp r
  emit ")"


fmtExp :: S.Exp -> FormatM ()
fmtExp (S.Let name value body) = do
  emit "let "
  emit name
  emit " = "
  fmtExp value
  newline 4
  emit "in "
  fmtExp body
  indent $ -4
  return ()



fmtExp (S.Plus a b) = fmtBinary "+" a b
fmtExp (S.Minus a b) = fmtBinary "-" a b
fmtExp (S.Times a b) = fmtBinary "*" a b
fmtExp (S.Div a b) = fmtBinary "/" a b
fmtExp (S.Negate a) = emit "-" >> fmtExp a
fmtExp (S.Int i) = emit $ show i
fmtExp (S.Var s) = emit s


fmtExp (S.Lambda name body) = do
  emit "("
  fmtArg name
  emit " -> "
  fmtExp body
  emit ")"

fmtExp (S.App f a) = do
  emit "("
  fmtExp f
  emit " "
  fmtExp a
  emit ")"
  return ()


fmtArg :: S.Argument -> FormatM ()
fmtArg a = case a of
  S.Named s -> emit s
