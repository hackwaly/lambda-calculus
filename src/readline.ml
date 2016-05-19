type t =
  | Eval of Lambda.exp
  | Bind of string * Lambda.exp
