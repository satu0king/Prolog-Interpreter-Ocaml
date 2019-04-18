{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let leq = "<="
let sid = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let bid = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf                  }
  | '('          { Parser.LPAREN                     }
  | ')'          { Parser.RPAREN                     }
  | ':'          { Parser.COLON                      }
  | integer as s { Parser.INTEGER((int_of_string s)) }
  | sid as s      { Parser.SID(s)                      }
  | bid as s	{Parser.BID (s)}
  | '.'		 { Parser.DOT}
  | ','          { Parser.COMMA                      }
  | eof          { Parser.EOF                        }
{
}
