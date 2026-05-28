open Parser_core
open Parser_expr
open Parser_case_chain

let parse_switch p =
  let pos = p.pos in

  (* switch ( expr ) { casechains... } *)
  (* ^~~~~~                            *)
  let rec _switch p =
    match (peek p 0).kind with
    | Token.Identifier "switch" -> advance p 1 |> _lparen
    | Token.Identifier _ -> advance (add_error_value p "switch") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "switch") 1, None

  (* switch ( expr ) { casechains... } *)
  (*        ^                          *)
  and _lparen p =
    match (peek p 0).kind with
    | Token.LParen -> advance p 1 |> _expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* switch ( expr ) { casechains... } *)
  (*          ^~~~                     *)
  and _expr p =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p x

  (* switch ( expr ) { casechains... } *)
  (*               ^                   *)
  and _rparen p expr =
    match (peek p 0).kind with
    | Token.RParen -> _case_chains (advance p 1) expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* switch ( expr ) { case-chains... } *)
  (*                 ^~~~~~~~~~~~~~~~~~ *)
  and _case_chains p expr =
    match (peek p 0).kind with
    | Token.LCBrack ->
        let p, case_chains = advance p 1 |> _get_case_chains in
        (match case_chains with
        | None -> p, None
        | Some x ->
            let switch_ast : Ast.t = {
              kind = Ast.StmtSwitch { expression = expr; case_chains = x };
              pos = pos;
            } in
            p, Some switch_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LCBrack) 1, None

  and _get_case_chains p =
    match (peek p 0).kind with
    | Token.RCBrack -> advance p 1, Some []
    | Token.Eof -> add_error_eof p, None
    | _ ->
        let p, case_chain = parse_case_chain p in
        match case_chain with
        | None -> p, None
        | Some x ->
            let p, rest = _get_case_chains p in
            match rest with
            | None -> p, None
            | Some y -> p, Some (x :: y) in

  _switch p
