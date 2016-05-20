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

let unique_vars exp =
  let module VarSet = Set.Make(String) in
  let module VarMap = Map.Make(String) in
  let rec freevars ?(boundvars=VarSet.empty) exp = (
    match exp with
    | Var v -> (
      if VarSet.mem v boundvars then VarSet.empty
      else VarSet.add v VarSet.empty
    )
    | Apply (f, a) -> VarSet.union (freevars ~boundvars f) (freevars ~boundvars a)
    | Lambda (v, e) -> freevars ~boundvars:(VarSet.add v boundvars) e
  ) in
  let avoid = ref (freevars exp) in
  let sn = ref 0 in
  let rec var_of_int n = (
    let t = n mod 26 in
    let n = n / 26 in
    let s = String.make 1 (Char.chr ((Char.code 'a') + t)) in
    if n > 0 then var_of_int n ^ s else s:string
  ) in
  let rec gen_var () = (
    let var = var_of_int !sn in
    if VarSet.mem var !avoid then (
      sn := !sn + 1;
      gen_var ()
    ) else (
      avoid := VarSet.add var !avoid;
      var
    )
  ) in
  let unique_var var = (
    (* if VarSet.mem var !avoid then *)
      gen_var ()
    (* else var *)
  ) in
  let rec rename ?(map=VarMap.empty) exp = (
    match exp with
    | Var v -> (
      Var (try VarMap.find v map with Not_found -> v)
    )
    | Lambda (v, e) -> (
      let var = unique_var v in
      avoid := VarSet.add var !avoid;
      Lambda (var, rename ~map:(VarMap.add v var map) e)
    )
    | Apply (f, a) -> (
      let f = rename ~map f in
      let a = rename ~map a in
      Apply (f, a)
    )
  ) in
  rename exp

let notrace _ = ()

let rec normalize ?(trace=notrace) exp =
  let rec subst (v, a) e = (
    match e with
    | Lambda (v', e') when v' <> v -> Lambda (v', subst (v, a) e')
    | Apply (f, a') -> Apply (subst (v, a) f, subst (v, a) a')
    | Var v' when v' = v -> a
    | e' -> e'
  ) in
  let rec cbn e r = (
    match e with
    | Apply (f, a) -> (
      match cbn f (fun f -> r (Apply (f, a))) with
      | Lambda (v, e') -> (
        let e' = subst (v, a) e' in
        trace (r e');
        cbn e' r
      )
      | _ -> e
    )
    | _ -> e
  ) in
  let rec nor e r = (
    match e with
    | Lambda (v, e) -> (
      let e = nor e (fun e -> r (Lambda (v, e))) in
      Lambda (v, e)
    )
    | Apply (f, a) -> (
      match cbn f (fun f -> r (Apply (f, a))) with
      | Lambda (v, e') -> (
        let e' = subst (v, a) e' in
        trace (r e');
        nor e' r
      )
      | _ -> (
        let f = nor f (fun f -> r (Apply (f, a))) in
        let a = nor a (fun a -> r (Apply (f, a))) in
        Apply (f, a)
      )
    )
    | _ -> e
  ) in
  let exp = unique_vars exp in
  trace exp;
  let exp = nor exp (fun e -> e) in
  let exp = unique_vars exp in
  trace exp;
  exp
