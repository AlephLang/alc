open Parser_core
open Parser_stmt
open Parser_expr

let parse_switch p =
  let pos = p.pos in

  (* switch ( expr ) { cases... } *)
  (* ^~~~~~                       *)
  let rec _switch p =
    match (peek p 0).kind with
    | Token.Identifier "switch" -> advance p 1 |> _lparen
    | Token.Identifier _ -> advance (add_error_value p "switch") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "switch") 1, None

  (* switch ( expr ) { cases... } *)
  (*        ^                     *)
  and _lparen p =
    match (peek p 0).kind with
    | Token.LParen -> advance p 1 |> _expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* switch ( expr ) { cases... } *)
  (*          ^~~~                *)
  and _expr p =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p x

  (* switch ( expr ) { cases... } *)
  (*               ^              *)
  and _rparen p expr =
    match (peek p 0).kind with
    | Token.RParen -> _cases (advance p 1) expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* switch ( expr ) { cases... } *)
  (*                 ^~~~~~~~~~~~ *)
  and _cases p expr =
    match (peek p 0).kind with
    | Token.LCBrack ->
        let p, cases = advance p 1 |> _get_cases in
        (match cases with
        | None -> p, None
        | Some x ->
            let switch_ast : Ast.t = {
              kind = Ast.StmtSwitch { expression = expr; cases = x };
              pos = pos;
            } in
            p, Some switch_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LCBrack) 1, None

  and _get_cases p =
    (* case expr: block *)
    let __case p =
      let pos = p.pos in

      (* case expr: block *)
      (* ^~~~             *)
      let rec ___case p =
        match (peek p 0).kind with
        | Token.Identifier "case" -> advance p 1 |> ___expr
        | Token.Identifier _ -> advance (add_error_value p "case") 1, None
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p @@ Token.Identifier "case") 1, None

      (* case expr: block *)
      (*      ^~~~        *)
      and ___expr p =
        let p, expr = parse_expr p false in
        match expr with
        | None -> p, None
        | Some x -> ___colon p x

      (* case expr: block *)
      (*          ^       *)
      and ___colon p expr =
        let tok0 = peek p @@ -1 in
        match (peek p 0).kind with
        | Token.Colon when not tok0.has_whitespace_after -> ___block (advance p 1) expr
        | Token.Colon -> advance (add_error_whitespace (advance p @@ -1) Token.Colon) 1, None
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p Token.Colon) 1, None

      (* case expr: block *)
      (*            ^~~~~ *)
      and ___block p expr =
        let p, block = parse_stmt_block p in
        match block with
        | None -> p, None
        | Some x ->
            let case_ast : Ast.t = {
              kind = Ast.StmtCase { expression = expr; body = x };
              pos = pos;
            } in
            p, Some case_ast in

      ___case p in

    (* default: block *)
    let __default p =
      let pos = p.pos in

      (* default: block *)
      (* ^~~~~~~~       *)
      let rec ___default p =
        let tok1, tok2 = peek p 0, peek p 1 in
        match tok1.kind, tok2.kind with
        | Token.Identifier "default", Token.Colon when not tok1.has_whitespace_after ->
            advance p 2 |> ___block
        | Token.Identifier "default", Token.Colon ->
            advance (add_error_whitespace p Token.Colon) 1, None
        | Token.Identifier "default", Token.Eof -> add_error_eof (advance p 1), None
        | Token.Identifier "default", _ ->
            advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
        | Token.Identifier _, _ -> advance (add_error_value p "default") 1, None
        | Token.Eof, _ -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p @@ Token.Identifier "default") 1, None

      (* default: block *)
      (*          ^~~~~ *)
      and ___block p =
        let p, block = parse_stmt_block p in
        match block with
        | None -> p, None
        | Some x ->
            let default_ast : Ast.t = {
              kind = Ast.StmtDefault { body = x };
              pos = pos;
            } in
            p, Some default_ast in

      ___default p in

    match (peek p 0).kind with
    | Token.RCBrack -> advance p 1, Some []
    | _ ->
        let p, case =
          match (peek p 0).kind with
          | Token.Identifier "case" -> __case p
          | Token.Identifier "default" -> __default p
          | Token.Identifier _ -> advance (add_error_value_s p ["case"; "default"]) 1, None
          | Token.Eof -> add_error_eof p, None
          | _ -> advance (add_error_unexpected_s p [ Token.Identifier "case"
                                                   ; Token.Identifier "default"]) 1, None in
        match case with
        | None -> p, None
        | Some x ->
            let p, rest = _get_cases p in
            match rest with
            | None -> p, None
            | Some y -> p, Some (x :: y) in

  _switch p
