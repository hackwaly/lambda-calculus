
let main () =
  let lexbuf = Lexing.from_channel stdin in
  let module StringMap = Map.Make(String) in
  let map = ref StringMap.empty in
  let rec resolve exp = (
    let open Lambda in
    match exp with
    | Var v -> (try StringMap.find v !map with Not_found -> exp)
    | Apply (f, p) -> Apply (resolve f, resolve p)
    | Lambda (v, e) -> Lambda (v, resolve e)
  ) in
  try
    while true do
      let open Readline in
      match Parser.readline Lexer.token lexbuf with
      | Eval exp ->
        let exp = resolve exp in
        print_endline ("=> " ^ Lambda.string_of exp);
        let rec interp exp =
          match Lambda.beta_reduce exp with
          | Some exp -> (
            print_endline ("=> " ^ Lambda.string_of exp);
            Unix.sleep 1;
            interp exp
          )
          | None -> ()
        in
        interp exp
      | Bind (name, exp) -> (
        map := StringMap.add name exp !map
      )
    done
  with Lexer.Eof -> ()

;;
main ()
