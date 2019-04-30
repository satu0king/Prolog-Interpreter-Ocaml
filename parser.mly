%{
%}

%token          NEWLINE APOSTROPHE WS COMMA EOF LPAREN RPAREN COLON DOT L_SQUARE R_SQUARE
%token <int>    INTEGER
%token <string> STRING
%token <string> VARIABLE

%start database
%start filename
%start interpreter_query
%type <Expression.predicate> predicate
%type <Expression.value> value
%type <Expression.argument> argument
%type <Expression.query> query
%type <Expression.query> interpreter_query
%type <Expression.predicateName> predicateName
%type <Expression.predicate list> database
%type <string> filename

%left ADD SUBTRACT
%left AND OR
%right NOT

%% /* Grammar rules and actions follow */

database :
 database_r EOF {$1}
;

database_r:
| predicate					    {[$1]}
| predicate database 			{$1 :: $2}
;

predicate :
 | predicateName LPAREN argumentlist RPAREN DOT	                {Expression.Rule($1, $3, [])}
 | predicateName LPAREN argumentlist RPAREN COLON rulelist DOT  {Expression.Rule($1, $3, $6)}
;

predicateName:
 | STRING {Expression.PredicateName($1)}
;

argument:
 | VARIABLE {Expression.Variable($1)}
 | value    {Expression.Constant($1)}
;

value :
 | STRING    {Expression.Atom($1)}
 | INTEGER   {Expression.Integer($1)}
;

rulelist :
 | query			     		{[$1]}
 | query COMMA rulelist			{$1 :: $3}
;

interpreter_query:
 query DOT {$1}
;

query:
 | predicateName LPAREN argumentlist RPAREN {Expression.Query($1, $3)}
;

filename:
  | L_SQUARE APOSTROPHE STRING DOT STRING APOSTROPHE R_SQUARE DOT                         { $3 ^ "." ^ $5 }
;


argumentlist :
 | argument				             	{[$1]}
 | argument COMMA argumentlist			{$1 :: $3}
;
%%
