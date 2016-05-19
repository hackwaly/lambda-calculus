%{
open Lambda

%}

%token NL LAMBDA DOT WS LPAREN RPAREN
%token <string> VAR
%start <Lambda.exp> readline

%left WS

%%

readline:
  | _ e=exp _ NL { e }

exp0:
  | v=VAR { Var v }
  | LAMBDA _ v=VAR _ DOT _ e=exp0 { Lambda (v, e) }
  | LPAREN _ e=exp _ RPAREN { e }

exp:
  | f=exp WS p=exp { Apply (f, p) }
  | exp0 { $1 }

_: WS? {}
