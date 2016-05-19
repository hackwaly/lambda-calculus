
module Make
           (NatTrans: Eval.NatTrans)
= struct
  
  exception Error
  
  let _eRR =
    Error
  
  type token = Token.token
  
  type _menhir_env = {
    _menhir_lexer: Lexing.lexbuf -> token;
    _menhir_lexbuf: Lexing.lexbuf;
    _menhir_token: token;
    mutable _menhir_error: bool
  }
  
  and _menhir_state = 
    | MenhirState38
    | MenhirState34
    | MenhirState33
    | MenhirState32
    | MenhirState30
    | MenhirState28
    | MenhirState26
    | MenhirState24
    | MenhirState22
    | MenhirState21
    | MenhirState16
    | MenhirState15
    | MenhirState12
    | MenhirState11
    | MenhirState9
    | MenhirState7
    | MenhirState6
    | MenhirState3
    | MenhirState0
    
open Lambda
open Eval


  let rec _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Lambda.exp) -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | Token.LAMBDA ->
          _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
      | Token.LPAREN ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
      | Token.NAT _v ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
      | Token.VAR _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
      | Token.NL | Token.RPAREN ->
          _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16
  
  and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lambda.exp) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState6 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.WS ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | Token.RPAREN ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
      | MenhirState22 | MenhirState16 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (((_menhir_stack, _menhir_s, (f : (Lambda.exp))), _), _, (p : (Lambda.exp))) = _menhir_stack in
          let _2 = () in
          let _v : (Lambda.exp) =                    ( Apply (f, p) ) in
          _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
      | MenhirState0 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState21 in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (e : (Lambda.exp))) = _menhir_stack in
              let _2 = () in
              let _v : (Lambda.exp) =               ( e ) in
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_1 : (Lambda.exp)) = _v in
              Obj.magic _1
          | Token.WS ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState21 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | Token.LAMBDA ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
              | Token.LPAREN ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
              | Token.NAT _v ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
              | Token.VAR _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
      | MenhirState33 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.WS ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | Token.NL ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
      | MenhirState26 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.WS ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | Token.NL ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
      | _ ->
          _menhir_fail ()
  
  and _menhir_fail : unit -> 'a =
    fun () ->
      Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
      assert false
  
  and _menhir_goto_readline : _menhir_env -> 'ttv_tail -> _menhir_state -> (Eval.command) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = Obj.magic _menhir_stack in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (_1 : (Eval.command)) = _v in
      Obj.magic _1
  
  and _menhir_goto_exp0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lambda.exp) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      match _menhir_s with
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (e : (Lambda.exp)) = _v in
          let (((((_menhir_stack, _menhir_s), _, (_2 : (unit))), (v : (string))), _, (_4 : (unit))), _, (_6 : (unit))) = _menhir_stack in
          let _5 = () in
          let _1 = () in
          let _v : (Lambda.exp) =                                   ( Lambda (v, e) ) in
          _menhir_goto_exp0 _menhir_env _menhir_stack _menhir_s _v
      | MenhirState26 | MenhirState33 | MenhirState22 | MenhirState0 | MenhirState16 | MenhirState6 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_1 : (Lambda.exp)) = _v in
          let _v : (Lambda.exp) =          ( _1 ) in
          _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_option_WS_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = Obj.magic _menhir_stack in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (_1 : (unit option)) = _v in
      let _v : (unit) =        () in
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState3 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.LAMBDA ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
          | Token.LPAREN ->
              _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
          | Token.NAT _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
          | Token.VAR _v ->
              _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
      | MenhirState7 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.VAR _v ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = (_menhir_stack, _v) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | Token.WS ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
              | Token.DOT ->
                  _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState9 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.DOT ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | Token.WS ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
              | Token.LAMBDA | Token.LPAREN | Token.NAT _ | Token.VAR _ ->
                  _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState11 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.LAMBDA ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
          | Token.LPAREN ->
              _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
          | Token.NAT _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
          | Token.VAR _v ->
              _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
      | MenhirState15 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((((_menhir_stack, _menhir_s), _, (_2 : (unit))), _, (e : (Lambda.exp))), _, (_4 : (unit))) = _menhir_stack in
              let _5 = () in
              let _1 = () in
              let _v : (Lambda.exp) =                             ( e ) in
              _menhir_goto_exp0 _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState24 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState26 in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
              let _2 = () in
              let _v : (Eval.command) =           ( Stop ) in
              _menhir_goto_readline _menhir_env _menhir_stack _menhir_s _v
          | Token.LAMBDA ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
          | Token.LET ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState26 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | Token.WS ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
              | Token.VAR _ ->
                  _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
          | Token.LPAREN ->
              _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
          | Token.NAT _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
          | Token.NL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState26 in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
              let _2 = () in
              let _v : (Eval.command) =          ( Nop ) in
              _menhir_goto_readline _menhir_env _menhir_stack _menhir_s _v
          | Token.VAR _v ->
              _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
      | MenhirState28 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.VAR _v ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = (_menhir_stack, _v) in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | Token.WS ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
              | Token.EQ ->
                  _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState30 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.EQ ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | Token.WS ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
              | Token.LAMBDA | Token.LPAREN | Token.NAT _ | Token.VAR _ ->
                  _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState32 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.LAMBDA ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
          | Token.LPAREN ->
              _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33
          | Token.NAT _v ->
              _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
          | Token.VAR _v ->
              _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
      | MenhirState34 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.NL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((((((((_menhir_stack, _menhir_s, (_1 : (unit))), _), _, (_3 : (unit))), (n : (string))), _, (_5 : (unit))), _, (_7 : (unit))), _, (e : (Lambda.exp))), _, (_9 : (unit))) = _menhir_stack in
              let _10 = () in
              let _6 = () in
              let _2 = () in
              let _v : (Eval.command) =                                     ( Bind (n, e) ) in
              _menhir_goto_readline _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState38 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Token.NL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (((_menhir_stack, _menhir_s, (_1 : (unit))), _, (e : (Lambda.exp))), _, (_3 : (unit))) = _menhir_stack in
              let _4 = () in
              let _v : (Eval.command) =                  ( Eval e ) in
              _menhir_goto_readline _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | _ ->
          _menhir_fail ()
  
  and _menhir_reduce10 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let (_menhir_stack, _menhir_s) = _menhir_stack in
      let x = () in
      let _v : (unit option) =     ( Some x ) in
      _menhir_goto_option_WS_ _menhir_env _menhir_stack _menhir_s _v
  
  and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (v : (string)) = _v in
      let _v : (Lambda.exp) =           ( Var v ) in
      _menhir_goto_exp0 _menhir_env _menhir_stack _menhir_s _v
  
  and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (n : (int)) = _v in
      let _v : (Lambda.exp) =           ( NatTrans.nat n  ) in
      _menhir_goto_exp0 _menhir_env _menhir_stack _menhir_s _v
  
  and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | Token.WS ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
      | Token.LAMBDA | Token.LPAREN | Token.NAT _ | Token.VAR _ ->
          _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3
  
  and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | Token.WS ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
      | Token.VAR _ ->
          _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7
  
  and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      match _menhir_s with
      | MenhirState38 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState34 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState33 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState32 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState30 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState28 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState26 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState24 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          raise _eRR
      | MenhirState22 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState21 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState16 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState15 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState11 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState9 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState7 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState6 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState3 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState0 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          raise _eRR
  
  and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _v : (unit option) =     ( None ) in
      _menhir_goto_option_WS_ _menhir_env _menhir_stack _menhir_s _v
  
  and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _menhir_env = _menhir_discard _menhir_env in
      _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
  
  and _menhir_discard : _menhir_env -> _menhir_env =
    fun _menhir_env ->
      let lexer = _menhir_env._menhir_lexer in
      let lexbuf = _menhir_env._menhir_lexbuf in
      let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }
  
  and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
    fun lexer lexbuf ->
      let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }
  
  and parse_exp : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lambda.exp) =
    fun lexer lexbuf ->
      let _menhir_env = _menhir_init lexer lexbuf in
      Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | Token.LAMBDA ->
          _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
      | Token.LPAREN ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
      | Token.NAT _v ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
      | Token.VAR _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  
  and readline : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Eval.command) =
    fun lexer lexbuf ->
      let _menhir_env = _menhir_init lexer lexbuf in
      Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | Token.WS ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
      | Token.EOF | Token.LAMBDA | Token.LET | Token.LPAREN | Token.NAT _ | Token.NL | Token.VAR _ ->
          _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    


end
