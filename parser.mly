%{
%}

%token          NEWLINE WS COMMA EOF LPAREN RPAREN COLON DOT
%token <int>    INTEGER
%token <string> ATOMNAME
%token <string> VARIABLENAME
%token <string> FACTNAME

%start predicate
%type <Expression.predicate> predicate
%type <Expression.value> value
%type <Expression.argument> argument
%type <Expression.query> query
%type <Expression.predicateName> predicateName

%left ADD SUBTRACT
%left AND OR
%right NOT

%% /* Grammar rules and actions follow */


predicate :
 | predicateName LPAREN argumentlist RPAREN DOT	{Expression.Rule($1, $3, [])}
 | predicateName LPAREN argumentlist RPAREN COLON rulelist DOT	{Expression.Rule ($1, $3, $6)}
;

argument:
 | VARIABLENAME {Expression.Variable($1)}
 | value {Expression.Constant($1)}
;

value :
 | ATOMNAME {Expression.Atom($1)}
 | INTEGER {Expression.Integer($1)}
;

rulelist :
 | query					{[$1]}
 | query COMMA rulelist			{$1 :: $3}
;

query:
 | predicateName LPAREN argumentlist RPAREN {Expression.Query($1, $3)}
;

predicateName:
 | FACTNAME {Expression.PredicateName($1)}
;

argumentlist :
 | argument					{[$1]}
 | argument COMMA argumentlist			{$1 :: $3}
;
%%
