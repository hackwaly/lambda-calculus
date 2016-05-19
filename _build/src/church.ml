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
    "plus",  parse_exp "λm.λn.λf.λx.(m f (n f x))";
    "succ",  parse_exp "λm.λn.λf.λx.(m f (n f x)) 1";
    "mult",  parse_exp "λm.λn.λf.(n (m f))";
    "true",  parse_exp "λa.λb.a";
    "false", parse_exp "λa.λb.b";
  ]
