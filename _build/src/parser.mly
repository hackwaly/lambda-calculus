%{
open Lambda
open Eval

%}

%parameter <NatTrans: Eval.NatTrans>

%token EOF NL LAMBDA DOT WS LPAREN RPAREN EQ LET
%token <string> VAR
%token <int> NAT

%start <Eval.command> readline
%start <Lambda.exp> parse_exp

%left WS

%%

readline:
  | _ e=exp _ NL { Eval e }
  | _ LET _ n=VAR _ EQ _ e=exp _ NL { Bind (n, e) }
  | _ NL { Nop }
  | _ EOF { Stop }

parse_exp:
  | e=exp EOF { e }

exp0:
  | v=VAR { Var v }
  | LAMBDA _ v=VAR _ DOT _ e=exp0 { Lambda (v, e) }
  | LPAREN _ e=exp _ RPAREN { e }
  | n=NAT { NatTrans.nat n  }

exp:
  | f=exp WS p=exp { Apply (f, p) }
  | exp0 { $1 }

_: WS? {}
