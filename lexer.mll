{
exception Lexer_exception of string
}

let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let string = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let variable = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf                  }
  | '('          { Parser.LPAREN                     }
  | ')'          { Parser.RPAREN                     }
  | ":-"          { Parser.COLON                      }
  | string as s	{Parser.STRING (s)}
  | integer as s { Parser.INTEGER((int_of_string s)) }
  | variable as s	{Parser.VARIABLE (s)}
  | '.'		 { Parser.DOT}
  | ','          { Parser.COMMA                      }
  | eof          { Parser.EOF                        }
  | '['             { Parser.L_SQUARE }
  | ']'             { Parser.R_SQUARE }
  | "'"             { Parser.APOSTROPHE }
{
}
