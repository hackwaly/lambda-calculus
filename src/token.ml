type token =
  | EOF | NL | LAMBDA | DOT | LPAREN | RPAREN | EQ | LET
  | VAR of string
  | NAT of int
