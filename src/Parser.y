{
module Parser where
import Lexer
import Syntax
}

%name parseFutz
%tokentype { Token }
%error { parseError }

%token
    -- Match syntax
    let  { Tok _ TSyntax "let" }
    in   { Tok _ TSyntax "in"}
    of   { Tok _ TSyntax "of"}
    -- Literals
    int  { Tok _ TInt $$ }
    var  { Tok _ TSym $$ }
    -- Symbols
    '='  { Tok _ TEq  _}
    '+'  { Tok _ TPlus _ }
    '-'  { Tok _ TMinus _ }
    '*'  { Tok _ TTimes _ }
    '/'  { Tok _ TDiv _ }
    '('  { Tok _ TLParen _ }
    ')'  { Tok _ TRParen _ }
    arr  { Tok _ TArrow _ }
    'λ'  { Tok _ TLambda _ }

%right APP

-- %left arr
-- %right in
-- %nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG



%%

exp
  : let var '=' exp in exp      { Let $2 $4 $6 }
  | let var args '=' exp in exp { Let $2 (expandLambdaArguments $3 $5) $7 }
  | 'λ' args arr exp            { expandLambdaArguments $2 $4 }
  | expapp                      { $1 }
  | exp of exp                  { App $1 $3 }
  | exp '+' exp                 { Plus $1 $3 }
  | exp '-' exp                 { Minus $1 $3 }
  | exp '*' exp                 { Times $1 $3 }
  | exp '/' exp                 { Div $1 $3 }
  | '-' exp %prec NEG           { Negate $2 }
  | int                         { Int (read $1) }
  | var                         { Var $1 }

expapp
  : expapp atom             { App $1 $2 }
  | atom                     { $1 }


atom
  : int                           { Int (read $1) }
  | var                           { Var $1 }
  | '(' exp ')'                   { $2 }


argument : var                    { Named $1 }

-- TODO: argument 
args : argument                   { [$1] }
     | argument args              { $1 : $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

expandLambdaArguments :: [Argument] -> Exp -> Exp
expandLambdaArguments (x:[]) body = Lambda x body
expandLambdaArguments (x:xs) body = Lambda x (expandLambdaArguments xs body)

}
