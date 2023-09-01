{
module Parser where
import Lexer
import Syntax
}

%name parseFutz
%tokentype { Token }
%error { parseError }

%token
    let  { TLet }
    in   { TIn }
    int  { TInt $$ }
    var  { TSym $$ }
    '='  { TEq }
    '+'  { TPlus }
    '-'  { TMinus }
    '*'  { TTimes }
    '/'  { TDiv }
    '('  { TLParen }
    ')'  { TRParen }
    arr  { TArrow }
    'λ'  { TLambda }
    'of' { TOf }


%left arr

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%nonassoc var
%nonassoc APP
%nonassoc of
%%

Exp : let var '=' Exp in Exp      { Let $2 $4 $6 }
    | let var args '=' Exp in Exp { Let $2 (expandLambdaArguments $3 $5) $7 }
    | args arr Exp                { expandLambdaArguments $1 $3 }
    | Exp Exp          %prec APP  { App $1 $2 }
    | Exp 'of' Exp     %prec APP  { App $1 $3 }
    | Exp '+' Exp                 { Plus $1 $3 }
    | Exp '-' Exp                 { Minus $1 $3 }
    | Exp '*' Exp                 { Times $1 $3 }
    | Exp '/' Exp                 { Div $1 $3 }
    | '(' Exp ')'                 { $2 }
    | '-' Exp %prec NEG           { Negate $2 }
    | int                         { Int $1 }
    | var                         { Var $1 }

argument : var                    { Named $1 }

-- TODO: argument 
args : argument                   { [$1] }
     | argument args              { $1 : $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
