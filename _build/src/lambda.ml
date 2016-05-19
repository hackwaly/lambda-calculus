type var = string
type exp =
  | Var of var
  | Lambda of var * exp
  | Apply of exp * exp

let rec string_of ?(paren_free=true) exp =
  let auto_paren s =
    if paren_free then s else
    "(" ^ s ^ ")"
  in
  match exp with
  | Lambda (v, e) ->
    "λ" ^ string_of (Var v) ^ "." ^ string_of ~paren_free:false e
  | Var v -> v
  | Apply (f, p) -> auto_paren (
    string_of f ^ " " ^ string_of ~paren_free:false p
  )

let rec substitute (v, p) e =
  match e with
  | Lambda (v', e') when v' <> v -> Lambda (v', substitute (v, p) e')
  | Apply (f, p') -> Apply (substitute (v, p) f, substitute (v, p) p')
  | Var v' when v' = v -> p
  | e' -> e'


exception Beta_reduction of exp

let rec beta_reduce e =
  let rec reduce e r = (
    match e with
    | Apply (f, p) -> (
      let f = reduce f (fun e -> r (Apply (e, p))) in
      match f with
      | Lambda (v, e) ->
        let e = substitute (v, p) e in
        raise (Beta_reduction (r e))
      | _ -> Apply (f, p)
    )
    | Lambda (v, e') -> (
      let e' = reduce e' (fun e -> r (Lambda (v, e))) in
      Lambda (v, e')
    )
    | e -> e
  ) in
  try ignore (reduce e (fun e -> e)); None
  with Beta_reduction e -> Some e
