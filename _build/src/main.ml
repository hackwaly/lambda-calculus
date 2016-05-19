let main () =
  let lexbuf = Lexing.from_channel stdin in
  let module Parser = Parser.Make (Church) in
  let parse_exp str = Parser.parse_exp Lexer.token (Lexing.from_string str) in
  let ctx = Eval.create ~globals:(Church.init_globals parse_exp) () in
  try
    while true do
      print_string "λ> ";
      flush stdout;
      let cmd = Parser.readline Lexer.token lexbuf in
      Eval.execute ctx cmd
    done
  with Eval.Stop -> ()

;;
main ()
