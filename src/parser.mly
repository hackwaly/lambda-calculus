%{
open Lambda
open Readline
%}

%token NL LAMBDA DOT WS LPAREN RPAREN EQ LET
%token <string> VAR
%start <Readline.t> readline

%left WS

%%

readline:
  | _ e=exp _ NL { Eval e }
  | _ LET _ n=VAR _ EQ _ e=exp _ NL { Bind (n, e) }

exp0:
  | v=VAR { Var v }
  | LAMBDA _ v=VAR _ DOT _ e=exp0 { Lambda (v, e) }
  | LPAREN _ e=exp _ RPAREN { e }

exp:
  | f=exp WS p=exp { Apply (f, p) }
  | exp0 { $1 }

_: WS? {}
