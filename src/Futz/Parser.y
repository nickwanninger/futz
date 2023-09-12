{
module Futz.Parser where
import Futz.Lexer
import qualified Futz.Types as T
import Futz.Syntax
}

%name parseFutz
%tokentype { Token }
%error { parseError }

%token

    -- Match syntax
    let  { Tok _ LSyntax "let" }
    in   { Tok _ LSyntax "in" }
    of   { Tok _ LSyntax "of" }
    if   { Tok _ LSyntax "if" }
    then { Tok _ LSyntax "then" }
    else { Tok _ LSyntax "else" }
    data { Tok _ LSyntax "data" }

    pipe   { Tok _ LPipe _ }
    '=>'   { Tok _ LOp "=>" }

    -- Literals
    int  { Tok _ LInt _ }
    var  { Tok _ LSym _ }
    op   { Tok _ LOp _ }
    -- Symbols
    '='      { Tok _ LEq  _}
    -- '+'      { Tok _ LPlus _ }
    -- '-'      { Tok _ LMinus _ }
    -- '*'      { Tok _ LTimes _ }
    -- '/'      { Tok _ LDiv _ }
    '('      { Tok _ LLParen _ }
    ')'      { Tok _ LRParen _ }
    arr      { Tok _ LArrow _ }
    'λ'      { Tok _ LLambda _ }
    tname    { Tok _ LType $$ }
    tvar     { Tok _ LTypeVar $$ }
    "::"     { Tok _ LIsType _ }
    -- Magic stuff inserted by the tokenizer
    sol      { TStartOfLine }

%right APP

-- %left arr
-- %right in
-- %nonassoc '>' '<'
-- %left '+' '-'
-- %left '*' '/'
%left NEG



%%

toplevel : statement            { [$1] }
         | statement toplevel   { $1 : $2 }

statement
  : sol var "::" qualtype                     { TypeDecl (tokVal $2) $4 }
  | sol '(' op ')' "::" qualtype              { TypeDecl (tokVal $3) $6 }
  | sol var '=' exp                           { Decl (tokVal $2) $4 }
  | sol '(' op ')' '=' exp                    { Decl (tokVal $3) $6 }
  | sol data tname listof(tvar) '=' ctors     { DataDecl $3 $4 $6 }
  -- | sol var args '=' exp        { Decl $2 (expandLambdaArguments $3 $5) }

exp :: { Exp SourceRange }
exp
  : let var '=' exp in exp          { Let (toRange $1 $6) (tokVal $2) $4 $6 }
  | if exp then exp else exp        { IfElse (toRange $1 $6) $2 $4 $6 }
  -- | let var args '=' exp in exp            { Let $2 (expandLambdaArguments $3 $5) $7 }
  | 'λ' unmatching_args arr exp     { expandLambdaArguments $2 (toRange $1 $4) $4 }
  | var                             { Var (toRange $1 $1) (tokVal $1) }
  | expapp                          { $1 }
  | atom of exp                     { App (toRange $1 $3) $1 $3 }
  | atom op exp                     { Inf (toRange $1 $3) (tokVal $2) $1 $3 }
  | atom                            { $1 }
  | op                              { Var (toRange $1 $1) (tokVal $1) }

expapp :: { Exp SourceRange }
expapp
  : expapp atom                   { App (toRange $1 $2) $1 $2 }
  | atom                          { $1 }

atom :: { Exp SourceRange }
atom
  : int                           { Lit (toRange $1 $1) (LitInt (read (tokVal $1))) }
  | var                           { Var (toRange $1 $1) (tokVal $1) }
  | '(' op ')'                    { Var (toRange $2 $2) (tokVal $2) }
  | '(' exp ')'                   { $2 }


argument :: { Argument }
argument : var                    { Named (tokVal $1) }

args :: { [Argument] }
args : argument                   { [$1] }
     | argument args              { $1 : $2 }


unmatching_args :: { [Var] }
unmatching_args : var                   { [tokVal $1] }
                | var unmatching_args   { (tokVal $1) : $2 }


typeParameters : simpleType                   { [$1] }
               | simpleType typeParameters    { $1 : $2 }


-- A simpleType is a type that is either a single name, or
-- a complex type wrapped in parens
-- (arrows are not simple types)
simpleType :: { T.Type }
simpleType : tname                { T.TCon (T.Tycon $1 T.Star) } 
           | tvar                 { T.TVar (T.Tyvar (tail $1) T.Star) }
           | '(' type ')'         { $2 }

type :: { T.Type }
type : simpleType                 { $1 } -- T.TCon (T.Tycon $1 T.Star) }
     | tApp                       { $1 }
     | type arr type              { T.fn $1 $3 }

tApp :: { T.Type }
tApp : simpleType                 { $1 }
     | tApp simpleType            { T.TAp $1 $2 }


ctor :: { Constructor }
ctor : tname listof(simpleType)   { Constructor $1 $2 }
     | tname                      { Constructor $1 [] }

ctors :: { [Constructor] }
ctors : ctor                      { [$1] }
      | ctor pipe ctors           { $1 : $3 }


qualtype :: { T.Qual T.Type }
qualtype : preds '=>' type         { $1 T.:=> $3 }
         | type                    { [] T.:=> $1 } 

preds :: { [T.Pred] }
preds : pred                      { [$1] }
      | pred pipe preds           { $1 : $3 }

pred :: { T.Pred }
pred : tname simpleType           { T.IsIn $1 $2 }


listof(p) : p                   { [$1] }
          | p listof(p)         { $1 : $2 }

{


toRange :: (Locatable a, Locatable b) => a -> b -> SourceRange
toRange a b = mergeRange (locate a) (locate b)


parseError :: [Token] -> a
parseError ((Tok (Pos line col) tc raw):ts)
  = error $ "Parse error near '" <> raw <> "' at line " <> (show line) <> " column " <> (show col)
parseError e = error $ "Unexplained parse error: " <> (show e)

expandLambdaArguments :: [String] -> SourceRange -> Exp SourceRange -> Exp SourceRange
expandLambdaArguments (x:[]) sr body = Lambda sr x body
expandLambdaArguments (x:xs) sr body = Lambda sr x (expandLambdaArguments xs sr body)

}
