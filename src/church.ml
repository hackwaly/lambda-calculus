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
    "plus",  parse_exp "λm.λn.λf.λx. m f (n f x)";
    "succ",  parse_exp "λn.λf.λx. f (n f x)";
    "mult",  parse_exp "λm.λn.λf. n (m f)";
    "exp",   parse_exp "λm.λn. n m";
    "pred",  parse_exp "λn.λf.λx. n (λg.λh. h (g f)) (λu. x) (λu. u)";
    "true",  parse_exp "λa.λb. a";
    "false", parse_exp "λa.λb. b";
    "and",   parse_exp "λm.λn.λa.λb. m (n a b) b";
    "or",    parse_exp "λm.λn.λa.λb. m a (n a b)";
    "not",   parse_exp "λm.λa.λb. m b a";
  ]
