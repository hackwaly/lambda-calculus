{
open Token
}

let alpha = ['A'-'Z' 'a'-'z']

rule token = parse
  | eof { EOF }
  | [' ' '\t']+ { token lexbuf }
  | '\r'? '\n' { NL }
  | "let" { LET }
  | "λ" | "\\" { LAMBDA }
  | alpha (alpha | ['_' '0'-'9'])* as m { VAR m }
  | ['0'-'9']+ as m { NAT (int_of_string m) }
  | "." { DOT }
  | "=" { EQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
