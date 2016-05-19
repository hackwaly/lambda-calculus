
module Make
           (NatTrans: Eval.NatTrans)
: sig
  
  (* The type of tokens. *)
  
  type token = Token.token
  
  (* This exception is raised by the monolithic API functions. *)
  
  exception Error
  
  (* The monolithic API. *)
  
  val readline: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Eval.command)
  
  val parse_exp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lambda.exp)
  
end
