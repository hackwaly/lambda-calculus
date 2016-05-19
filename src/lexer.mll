{
open Parser

exception Eof
}

let alpha = ['A'-'Z' 'a'-'z']

rule token = parse
  | eof { raise Eof }
  | '\r'? '\n' { NL }
  | "let" { LET }
  | "λ" | "\\" { LAMBDA }
  | alpha (alpha | ['_' '0'-'9'])* as m { VAR m }
  | "." { DOT }
  | "=" { EQ }
  | [' ' '\t']+ { WS }
  | "(" { LPAREN }
  | ")" { RPAREN }
