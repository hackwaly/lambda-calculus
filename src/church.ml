open Lambda

module StringMap = Map.Make(String)

let nat n =
  let rec tail = function
  | 0 -> Var "x"
  | n -> Apply (Var "f", tail (n - 1))
  in
  Lambda ("f", Lambda ("x", tail n))

let init_globals parse_exp =
  List.fold_left (
    fun globals (k, v) ->
    StringMap.add k v globals
  ) StringMap.empty [
    "plus",     parse_exp "λm.λn.λf.λx. m f (n f x)";
    "succ",     parse_exp "λn.λf.λx. f (n f x)";
    "mult",     parse_exp "λm.λn.λf. n (m f)";
    "exp",      parse_exp "λm.λn. n m";
    "pred",     parse_exp "λn.λf.λx. n (λg.λh. h (g f)) (λu. x) (λu. u)";
    "minus",    parse_exp "λm.λn. n pred m";
    "true",     parse_exp "λa.λb. a";
    "false",    parse_exp "λa.λb. b";
    "and",      parse_exp "λp.λq. p q p";
    "or",       parse_exp "λp.λq. p p q";
    "not",      parse_exp "λp. p false true";
    "if",       parse_exp "λp.λa.λb. p a b";
    "is_zero",  parse_exp "λn. n (λx. false) true";
    "leq",      parse_exp "λm.λn. is_zero (minus m n)";
    "eq",       parse_exp "λm.λn. and (leq m n) (leq n m)";
    "pair",     parse_exp "λx.λy.λz. z x y";
    "first",    parse_exp "λp. p (λx.λy. x)";
    "second",   parse_exp "λp. p (λx.λy. y)";
    "nil",      parse_exp "pair true true";
    "is_nil",   parse_exp "first";
    "cons",     parse_exp "λh.λt. pair false (pair h t)";
    "head",     parse_exp "λz. first (second z)";
    "tail",     parse_exp "λz. second (second z)";
    "Y",        parse_exp "λf. (λx. x x) (λx. f (x x))";
  ]
