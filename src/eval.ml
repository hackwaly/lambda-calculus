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
  | Var v -> (try resolve bindings (StringMap.find v bindings) with Not_found -> exp)
  | Apply (f, a) -> Apply (resolve bindings f, resolve bindings a)
  | Lambda (v, e) ->
    let bindings = StringMap.remove v bindings in
    Lambda (v, resolve bindings e)

let execute ctx cmd =
  let print_exp exp =
    print_endline ("=> " ^ Lambda.string_of exp);
    Unix.sleep 0;
  in
  match cmd with
  | Eval exp ->
    let exp = resolve ctx.globals exp in
    ignore (Lambda.normalize ~trace:print_exp exp)
  | Bind (name, exp) -> (
    ctx.globals <- StringMap.add name exp ctx.globals
  )
  | Stop -> raise Stop
  | Nop -> ()
