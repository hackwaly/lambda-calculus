open Lambda

module StringMap = Map.Make(String)

type nat = int

module type NatTrans = sig
  val nat : nat -> exp
end

type command =
  | Eval of Lambda.exp
  | Bind of string * Lambda.exp
  | Nop
  | Stop

exception Stop

type context = {
  mutable globals: exp StringMap.t;
}

let create ?(globals) () = {
  globals = (
    match globals with
    | None -> StringMap.empty
    | Some globals -> globals
  );
}

let rec resolve bindings exp =
  match exp with
  | Var v -> (try StringMap.find v bindings with Not_found -> exp)
  | Apply (f, p) -> Apply (resolve bindings f, resolve bindings p)
  | Lambda (v, e) ->
    let bindings = StringMap.remove v bindings in
    Lambda (v, resolve bindings e)

let execute ctx cmd =
  match cmd with
  | Eval exp ->
    let exp = resolve ctx.globals exp in
    print_endline ("=> " ^ Lambda.string_of exp);
    let rec interp exp =
      match Lambda.beta_reduce exp with
      | Some exp -> (
        print_endline ("=> " ^ Lambda.string_of exp);
        Unix.sleepf 0.016;
        interp exp
      )
      | None -> ()
    in
    interp exp
  | Bind (name, exp) -> (
    ctx.globals <- StringMap.add name exp ctx.globals
  )
  | Stop -> raise Stop
  | Nop -> ()
