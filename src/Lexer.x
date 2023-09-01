{
module Lexer where

import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \s -> TLet }
  in                            { \s -> TIn }
  of                            { \s -> TOf }
  $digit+                       { \s -> TInt (read s) }
  \=                            { \s -> TEq }
  \+                            { \s -> TPlus }
  \-                            { \s -> TMinus }
  \*                            { \s -> TTimes }
  \/                            { \s -> TDiv }
  \(                            { \s -> TLParen }
  \)                            { \s -> TRParen }
  \->                           { \s -> TArrow }
  λ                            { \s -> TLambda }
  \\                           { \s -> TLambda }
  $alpha [$alpha $digit \_ \']* { \s -> TSym s }

{

scanTokens = alexScanTokens

}
