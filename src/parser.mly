%{
open Lambda
open Eval

%}

%parameter <NatTrans: Eval.NatTrans>

%token EOF NL LAMBDA DOT LPAREN RPAREN EQ LET
%token <string> VAR
%token <int> NAT

%start <Eval.command> readline
%start <Lambda.exp> parse_exp

%%

readline:
  | e=exp NL { Eval e }
  | LET n=VAR EQ e=exp NL { Bind (n, e) }
  | NL { Nop }
  | EOF { Stop }

parse_exp:
  | e=exp EOF { e }

exp:
  | e=exp2 { e }

exp0:
  | v=VAR { Var v }
  | LPAREN e=exp RPAREN { e }
  | n=NAT { NatTrans.nat n  }

exp1:
  | e=exp0 { e }
  | f=exp1 a=exp0 { Apply (f, a) }

exp2:
  | e=exp1 { e }
  | LAMBDA v=VAR DOT e=exp2 { Lambda (v, e) }
