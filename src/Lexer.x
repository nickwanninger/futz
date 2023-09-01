
{
module Lexer where
import Data.Char (chr)
import Syntax
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
	let|in|of

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special \:\"\']


tokens :-
  $white+                       ;
  "--".*                        ;
  @syntax                       { mkL TSyntax }
  $digit+                       { mkL TInt }
  \=                            { mkL TEq }
  \+                            { mkL TPlus }
  \-                            { mkL TMinus }
  \*                            { mkL TTimes }
  \/                            { mkL TDiv }
  \(                            { mkL TLParen }
  \)                            { mkL TRParen }
  \->                           { mkL TArrow }
  λ                             { mkL TLambda }
  \\                            { mkL TLambda }
  $alpha [$alpha $digit \_ \']* { mkL TSym }
-- $octit	   = 0-7
-- $hexit     = [0-9 A-F a-f]
-- $idchar    = [$alpha $digit \']
-- $symchar   = [$symbol \:]
-- $nl        = [\n\r]

-- @reservedid = 
-- 	as|case|class|data|default|deriving|do|else|hiding|if|
-- 	import|in|infix|infixl|infixr|instance|let|module|newtype|
-- 	of|qualified|then|type|where

-- @reservedop =
-- 	".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"
--
-- @varid  = $small $idchar*
-- @conid  = $large $idchar*
-- @varsym = $symbol $symchar*
-- @consym = \: $symchar*
--
-- @decimal     = $digit+
-- @octal       = $octit+
-- @hexadecimal = $hexit+
-- @exponent    = [eE] [\-\+] @decimal
--
-- $cntrl   = [$large \@\[\\\]\^\_]
-- @ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
-- 	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
-- 	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
-- 	 | SUB | ESC | FS | GS | RS | US | SP | DEL
-- $charesc = [abfnrtv\\\"\'\&]
-- @escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
-- @gap     = \\ $whitechar+ \\
-- @string  = $graphic # [\"\\] | " " | @escape | @gap
--
-- haskell :-
--
-- <0> $white+			{ skip }
-- <0> "--"\-*[^$symbol].*		{ skip }
--
-- <0> $special			{ mkL LSpecial }
--
-- <0> @reservedid			{ mkL LReservedId }
-- <0> @conid \. @varid		{ mkL LQVarId }
-- <0> @conid \. @conid		{ mkL LQConId }
-- <0> @varid			{ mkL LVarId }
-- <0> @conid			{ mkL LConId }
--
-- <0> @reservedop			{ mkL LReservedOp }
-- <0> @conid \. @varsym		{ mkL LVarSym }
-- <0> @conid \. @consym		{ mkL LConSym }
-- <0> @varsym			{ mkL LVarSym }
-- <0> @consym			{ mkL LConSym }
--
-- <0> @decimal 
--   | 0[oO] @octal
--   | 0[xX] @hexadecimal		{ mkL LInteger }
--
-- <0> @decimal \. @decimal @exponent?
--   | @decimal @exponent		{ mkL LFloat }
--
-- <0> \' ($graphic # [\'\\] | " " | @escape) \'
-- 				{ mkL LChar }
--
-- <0> \" @string* \"		{ mkL LString }

{
  
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p,_,_,str) len = case p of 
  AlexPn _ line col -> return (Tok (Pos line col) c (take len str))


lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++ 
		   (if (not (null input))
		     then " before " ++ show (head input)
		     else " at end of file"))

scanTokens str = runAlex str $ do
  let loop i = do tok@(Tok _ cl _) <- alexMonadScan; 
		  if cl == TEOF
			then return i
			else do loop (i ++ [tok])
  loop []

alexEOF = return (Tok (Pos 0 0) TEOF "")

showPosn (AlexPn _ line col) = show line ++ ':': show col

}

