%{
%}

%token          NEWLINE WS COMMA EOF LPAREN RPAREN COLON DOT
%token <int>    INTEGER
%token <string> SID
%token <string> BID

%start expr
%type <Expression.expr> expr

%left ADD SUBTRACT
%left AND OR
%right NOT

%% /* Grammar rules and actions follow */

expr :	  	
 | fact			{$1}
;

fact :
 | SID LPAREN factlist RPAREN DOT	{Expression.Fact ($1, $3)}
;
factlist :
 | SID					{[$1]}
 | SID COMMA factlist			{$1 :: $3}
;
%%
