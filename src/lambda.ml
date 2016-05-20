type var = string
type exp =
  | Var of var
  | Lambda of var * exp
  | Apply of exp * exp

type hum_exp =
  | HumVar of var
  | HumSeq of var list * hum_exp list

let rec to_hum_exp exp =
  match exp with
  | Var v -> HumVar v
  | Lambda (v, e) -> (
    let he = to_hum_exp e in
    match he with
    | HumVar _ -> HumSeq (v :: [], he :: [])
    | HumSeq (pl, el) -> HumSeq (v :: pl, el)
  )
  | Apply (f, a) -> (
    let hf = to_hum_exp f in
    let ha = to_hum_exp a in
    match hf with
    | HumSeq ([], el) -> HumSeq ([], List.append el (ha :: []))
    | _ -> HumSeq ([], hf :: ha :: [])
  )

let rec string_of_hum_exp ?(paren_free=true) exp =
  match exp with
  | HumVar v -> v
  | HumSeq (pl, el) -> (
    assert (List.length el >= 1);
    let els = el
    |> List.map (fun e -> string_of_hum_exp ~paren_free:false e)
    |> String.concat " " in
    let str = match List.length pl with
    | 0 -> els
    | _ -> (
      let pls = pl
      |> List.map (fun p -> "λ" ^ p ^ ".")
      |> String.concat "" in
      pls ^ " " ^ els
    ) in
    let need_paren = not paren_free && (
      (List.length pl > 0) || (List.length el >= 2)
    ) in
    if need_paren then "(" ^ str ^ ")" else str
  )

let string_of exp =
  let he = to_hum_exp exp in
  string_of_hum_exp he

let rec normalize e =
  let rec subst (v, a) e = (
    match e with
    | Lambda (v', e') when v' <> v -> Lambda (v', subst (v, a) e')
    | Apply (f, a') -> Apply (subst (v, a) f, subst (v, a) a')
    | Var v' when v' = v -> a
    | e' -> e'
  ) in
  let rec cbn e = (
    match e with
    | Apply (f, a) -> (
      match cbn f with
      | Lambda (v, e') -> cbn (subst (v, a) e')
      | _ -> e
    )
    | _ -> e
  ) in
  let rec nor e = (
    match e with
    | Lambda (v, e) -> Lambda (v, nor e)
    | Apply (f, a) -> (
      match cbn f with
      | Lambda (v, e') -> nor (subst (v, a) e')
      | _ -> Apply (nor f, nor a)
    )
    | _ -> e
  ) in
  nor e
