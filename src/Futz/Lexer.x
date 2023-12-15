
{
module Futz.Lexer where
import Data.Char (chr)
import Futz.Syntax

import Futz.Lexer.Layout

-- A good example of a parser can be found here:
-- https://github.com/haskell/alex/blob/master/examples/haskell.x

}
%wrapper "monad"

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

@syntax = 
	def|in|if|then|else|data|match|let|some|class|end

@layout =
  do|on|of

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special \:\"\']
$idchar    = [$alpha $digit \_ \']

tokens :-
  [\ \t\n]+                       ;
  -- \n                            { mkL LNewline }
  "--".* \n                        ;
  @syntax                       { mkL LSyntax }
  @layout                       { mkL LStartLayout }
  $digit+                       { mkL LInt }
  \=                            { mkL LEq }
  -- \+                            { mkL LPlus }
  -- \-                            { mkL LMinus }
  -- \*                            { mkL LTimes }
  -- \/                            { mkL LDiv }
  \(                            { mkL LLParen }
  \)                            { mkL LRParen }
  \->                           { mkL LArrow }
  -- Explicit Layout
  \{                            { mkL LOpen }
  \}                            { mkL LClose }
  \;                            { mkL LSemi }

  λ                             { mkL LLambda }
  "fn"                          { mkL LLambda }
  \|                            { mkL LPipe }
  \\                            { mkL LLambda }
  \:                            { mkL LIsType }
  \::                           { mkL LIsType }
  $small $idchar*               { mkL LSym }
  $ascsymbol $ascsymbol*        { mkL LOp }
  $large $idchar*               { mkL LType }
  ' $small $idchar*             { mkL LTypeVar }
  -- TODO: remove this! This is for native calls
  \$ $small $idchar*            { mkL LSym }

{
  
mkL :: Lexeme -> AlexInput -> Int -> Alex Token
mkL c (p,_,_,str) len = case p of 
  AlexPn _ line col -> return (Tok (Pos line col) c (take len str))


lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++ 
       (if (not (null input))
         then " before " ++ show (head input)
         else " at end of file"))




scanLoop i = do
  tok <- alexMonadScan
  case tok of
    -- If we hit the EOF token, 
    TEndOfFile -> return i
    -- If a token is the first token on a line, prepend it with a "TStartOfLine" token
    -- (Tok (Pos _ 1) _ _) -> scanLoop (i ++ [TStartOfLine, tok])
    _ -> scanLoop (i ++ [tok])


-- This function parses the tokens directly from the source text
scanTokens str = runAlex str $ scanLoop []



lex :: String -> Either String [Token]
-- First, scan the raw tokens that the programmer
-- themselves wrote, then run that result through
-- `layout`, which will insert "virtual" tokens
-- (that they did not type) to enable indentation
-- sensitive layouts
lex str = scanTokens str >>= layout



alexEOF = return TEndOfFile

showPosn (AlexPn _ line col) = show line ++ ':': show col

}

