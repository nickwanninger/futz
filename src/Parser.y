{
module Parser where
import Lexer
import qualified Types as T
import Syntax
}

%name parseFutz
%tokentype { Token }
%error { parseError }

%token

    -- Match syntax
    let  { Tok _ LSyntax "let" }
    in   { Tok _ LSyntax "in"}
    of   { Tok _ LSyntax "of"}
    -- Literals
    int  { Tok _ LInt $$ }
    var  { Tok _ LSym $$ }
    -- Symbols
    '='  { Tok _ LEq  _}
    '+'  { Tok _ LPlus _ }
    '-'  { Tok _ LMinus _ }
    '*'  { Tok _ LTimes _ }
    '/'  { Tok _ LDiv _ }
    '('  { Tok _ LLParen _ }
    ')'  { Tok _ LRParen _ }
    arr  { Tok _ LArrow _ }
    'λ'  { Tok _ LLambda _ }
    typ  { Tok _ LType $$ }
    "::" { Tok _ LIsType _ }
    -- Magic stuff inserted by the tokenizer
    sol  { TStartOfLine }

%right APP

-- %left arr
-- %right in
-- %nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG



%%

toplevel : statement        { [$1] }
         | statement toplevel   { $1 : $2 }

statement
  : sol var "::" type           { TypeDecl $2 $4 }
  | sol var '=' exp             { Decl $2 $4 }
  -- | sol var args '=' exp        { Decl $2 (expandLambdaArguments $3 $5) }

exp
  : let var '=' exp in exp                 { Let $2 $4 $6 }
  -- | let var args '=' exp in exp            { Let $2 (expandLambdaArguments $3 $5) $7 }
  | 'λ' unmatching_args arr exp            { expandLambdaArguments $2 $4 }
  | expapp                                 { $1 }
  | exp of exp                             { App $1 $3 }
  | exp '+' exp                            { Plus $1 $3 }
  | exp '-' exp                            { Minus $1 $3 }
  | exp '*' exp                            { Times $1 $3 }
  | exp '/' exp                            { Div $1 $3 }
  | '-' exp %prec NEG                      { Negate $2 }
  | int                                    { Int (read $1) }
  | var                                    { Var $1 }

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



unmatching_args : var                   { [$1] }
                | var unmatching_args   { $1 : $2 }

type : type arr type              { T.TArrow $1 $3 }
     | typ                        { T.TNamed $1 }
     | '(' type ')'               { $2 }

{

parseError :: [Token] -> a
parseError ((Tok (Pos line col) tc raw):ts) = error $ "Parse error near '" <> raw <> "' at line " <> (show line) <> " column " <> (show col)
parseError e = error $ "Unexplained parse error: " <> (show e)

expandLambdaArguments :: [String] -> Exp -> Exp
expandLambdaArguments (x:[]) body = Lambda x body
expandLambdaArguments (x:xs) body = Lambda x (expandLambdaArguments xs body)

}
