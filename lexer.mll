{
exception Lexer_exception of string
}

let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let atomName = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let variableName = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let factName = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf                  }
  | '('          { Parser.LPAREN                     }
  | ')'          { Parser.RPAREN                     }
  | ":-"          { Parser.COLON                      }
  | integer as s { Parser.INTEGER((int_of_string s)) }
  | atomName as s      { Parser.ATOMNAME(s)                      }
  | variableName as s	{Parser.VARIABLENAME (s)}
  | factName as s	{Parser.FACTNAME (s)}
  | '.'		 { Parser.DOT}
  | ','          { Parser.COMMA                      }
  | eof          { Parser.EOF                        }
{
}
