{
open Parser
exception Eof
}

rule token = parse
  | eof { raise Eof }
  | '\r'? '\n' { NL }
  | "λ" | "\\" { LAMBDA }
  | ['a'-'z'] ['a'-'z' '0'-'9']* as m { VAR m }
  | "." { DOT }
  | [' ' '\t']+ { WS }
  | "(" { LPAREN }
  | ")" { RPAREN }
