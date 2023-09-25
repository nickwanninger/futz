{
module Futz.Parser where
import Futz.Lexer
import Futz.Types
import Futz.Syntax
}

%name parseFutz
%tokentype { Token }
%error { parseError }

%token

    -- Match syntax
    def      { Tok _ LSyntax "def" }
    let      { Tok _ LSyntax "let" }
    in       { Tok _ LSyntax "in" }
    if       { Tok _ LSyntax "if" }
    then     { Tok _ LSyntax "then" }
    else     { Tok _ LSyntax "else" }
    data     { Tok _ LSyntax "data" }
    match    { Tok _ LSyntax "match" }
    
    of       { Tok _ LStartLayout "of" }

    pipe   { Tok _ LPipe _ }
    '=>'   { Tok _ LOp "=>" }
    '#'   { Tok _ LOp "#" }

    -- Literals
    int      { Tok _ LInt _ }
    var      { Tok _ LSym _ }
    op       { Tok _ LOp _ }
    -- Symbols
    '='      { Tok _ LEq  _}
    -- '+'      { Tok _ LPlus _ }
    -- '-'      { Tok _ LMinus _ }
    -- '*'      { Tok _ LTimes _ }
    -- '/'      { Tok _ LDiv _ }
    '('      { Tok _ LLParen _ }
    ')'      { Tok _ LRParen _ }

    '->'     { Tok _ LArrow _ }
    'λ'      { Tok _ LLambda "λ" }

    '.'      { Tok _ LOp "." }

    fn       { Tok _ LLambda "fn" }
    tname    { Tok _ LType _ }
    -- tvar     { Tok _ LTypeVar $$ }
    "::"     { Tok _ LIsType _ }
    -- Magic stuff inserted by the tokenizer
    sol      { TStartOfLine }

    -- Explicit Layout
    Open     { Tok _ LOpen _ }
    Close    { Tok _ LClose _ }
    Semi     { Tok _ LSemi _ }
    -- Virtual Layout
    VOpen    { VTok LOpen }
    VClose   { VTok LClose }
    VSemi    { VTok LSemi }

%right APP

-- %left arr
-- %right in
-- %nonassoc '>' '<'
-- %left '+' '-'
-- %left '*' '/'
%left NEG



%%

-- top : layoutOf(statement)            { $1 }
top : listof(statement)            { $1 }

statement
  : def defnparts                           { TopDefn $2 }
  | data tname listof(var) '=' ctors        { DataDecl (tokVal $2) (map tokVal $3) $5 }
  | data tname '=' ctors                    { DataDecl (tokVal $2) [] $4 }

defnparts :: { [DefnPart SourceRange] }
defnparts : defnpart                       { [$1] }
          | defnpart pipe defnparts        { $1 : $3 }


defnpart :: { DefnPart SourceRange }
defnpart : var binding                   { DefnBind (tokVal $1) $2 }
         | pat0 op pat0 '=' exp          { DefnBind (tokVal $2) (Binding [$1, $3] $5)}
         | '(' op ')' binding            { DefnBind (tokVal $2) $4 }
         -- Type definitions
         | var "::" qtype                { DefnType (tokVal $1) $3 }
         | '(' op ')' "::" qtype         { DefnType (tokVal $2) $5 }


binding :: { Binding SourceRange }
: listof(pat0) '=' exp      { Binding $1 $3 }
| '=' exp                   { Binding [] $2 }


pat0 :: { Pat }
: var                               { PVar (tokVal $1) }
| '(' pat1 ')'                      { $2 }


pat1 :: { Pat }
: var                               { PVar (tokVal $1) }
| tname listof(pat0)                { PCon ((tokVal $1) :>: emptyScheme) $2 }
| tname                             { PCon ((tokVal $1) :>: emptyScheme) [] }


-------------------------------------------------------------------------------

exp :: { Exp SourceRange }

exp
: let letdefs in exp              { Let (toRange $1 $4) $2 $4 }
| if exp then exp else exp        { IfElse (toRange $1 $6) $2 $4 $6 }
-- | 'λ' var '.' exp                 { expandLambdaArguments (map (\x->[x]) (tokVal $2)) (toRange $1 $4) $4 }
| fn args0 '->' exp               { expandLambdaArguments $2 (toRange $1 $4) $4 }
| matchExp                        { $1 }
| var                             { Var (toRange $1 $1) (tokVal $1) }
| expapp                          { $1 }
-- | atom of exp                     { App (toRange $1 $3) $1 $3 }
| atom op exp                     { Inf (toRange $1 $3) (tokVal $2) $1 $3 }
| atom                            { $1 }
| op                              { Var (toRange $1 $1) (tokVal $1) }
| nativeCall                      { $1 }

expapp :: { Exp SourceRange }
expapp
: expapp atom                   { App (toRange $1 $2) $1 $2 }
| atom                          { $1 }

atom :: { Exp SourceRange }
atom
: int                           { Lit (toRange $1 $1) (LitInt (read (tokVal $1))) }
| var                           { Var (toRange $1 $1) (tokVal $1) }
-- TODO: give this it's own type!
| tname                         { Var (toRange $1 $1) (tokVal $1) }
| '(' op ')'                    { Var (toRange $2 $2) (tokVal $2) }
| '(' exp ')'                   { $2 }


nativeCall :: { Exp SourceRange }
: '#' '(' var "::" type pipe listofSep(exp, pipe) ')'
      { NativeCall (toRange $1 $8) (tokVal $3) $5 $7 }


letdefs :: { [Definition SourceRange] }
letdefs
: letdef                       { [$1] }
| letdef Semi letdefs          { $1 : $3 }

letdef :: { Definition SourceRange }
: var binding                  { Definition (tokVal $1) Nothing [$2] }



-------------------------------------------------------------------------------

matchExp :: { Exp SourceRange }
: match exp of layoutOf(matchArm)     { Match (toRange $1 $4) $2 $4 }

matchArm :: { MatchArm SourceRange }
: pat1 '=>' exp              { MatchArm $1 $3 }

-------------------------------------------------------------------------------

argument :: { Argument }
argument : var                    { Named (tokVal $1) }

args :: { [Argument] }
: argument              { [$1] }
| argument args         { $1 : $2 }

args0 :: { [Var] }
: var                   { [tokVal $1] }
| var args0             { (tokVal $1) : $2 }

-------------------------------------------------------------------------------


-- Handle function types
type :: { Type }
: btype '->' type     { fn $1 $3 }
| btype               { $1 }

-- Handle type application
btype :: { Type }
: btype atype          { TAp $1 $2 }
| atype                { $1 }

-- Handle base types (vars and parens)
atype :: { Type }
: var                  { TVar (Tyvar (tokVal $1) Star) }
| tname                { TCon (Tycon (tokVal $1) Star) } 
| '(' type ')'			   { $2 }
  -- TODO: https://github.com/wh5a/thih/blob/master/hatchet/HsParser.ly#L319


qtype :: { Qual Type }
: Open preds Close type   { $2 :=> $4 }
| type                    { [] :=> $1 }

----------------------------------------------------------------------------



ctor :: { Constructor }
ctor : tname listof(atype)        { Constructor (tokVal $1) $2 }
     | tname                      { Constructor (tokVal $1) [] }

ctors :: { [Constructor] }
ctors : ctor                      { [$1] }
      | ctor pipe ctors           { $1 : $3 }


preds :: { [Pred] }
preds : pred                      { [$1] }
      | pred pipe preds           { $1 : $3 }

pred :: { Pred }
pred : tname atype                { IsIn (tokVal $1) $2 }


-------------------------------------------------------------------------------

listof(p) : p                   { [$1] }
          | p listof(p)         { $1 : $2 }


listofSep(p, s)
: p                             { [$1] }
| p s listofSep(p, s)           { $1 : $3 }


layoutOf(p)
: VOpen listofSep(p, VSemi) VClose { $2 }
|  Open listofSep(p,  Semi)  Close { $2 }

-- LClose
-- : LVClose                     ; {- lexer inserted '}' -}
-- | error                       ; {- parse error generated '}' -}

{
emptyScheme :: Scheme
emptyScheme = quantify (tv t) ([] :=> t)
  where t = TGen 0

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
