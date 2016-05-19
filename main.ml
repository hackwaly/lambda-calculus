let main () =
  let lexbuf = Lexing.from_channel stdin in
  try
    while true do
      let exp = Parser.readline Lexer.token lexbuf in
      print_endline ("=> " ^ Lambda.string_of exp);
      let rec interp exp =
        match Lambda.beta_reduce exp with
        | Some exp -> (
          print_endline ("=> " ^ Lambda.string_of exp);
          interp exp
        )
        | None -> ()
      in
      interp exp
    done
  with Lexer.Eof -> ()

;;
main ()
