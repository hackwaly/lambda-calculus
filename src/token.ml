type token =
  | EOF | NL | LAMBDA | DOT | WS | LPAREN | RPAREN | EQ | LET
  | VAR of string
  | NAT of int
