open Parser_core
open Parser_expr
open Parser_stmt

let parse_case_chain p =
  let pos = p.pos in

  let rec _cases p =
    let pos = p.pos in

    let __case p =
      (* case expr: *)
      (* ^~~~       *)
      let rec ___case p =
        match (peek p 0).kind with
        | Token.Identifier "case" -> advance p 1 |> ___expr
        | Token.Identifier _ -> advance (add_error_value p "case") 1, None
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p @@ Token.Identifier "case") 1, None

      (* case expr: *)
      (*      ^~~~  *)
      and ___expr p =
        let p, expr = parse_expr p false in
        match expr with
        | None -> p, None
        | Some x -> ___colon p x

      (* case expr: *)
      (*          ^ *)
      and ___colon p expr =
        let tok0, tok1 = peek p @@ -1, peek p 0 in
        match tok1.kind with
        | Token.Colon when not tok0.has_whitespace_after ->
            let case_ast : Ast.t = {
              kind = Ast.Case { expression = expr };
              pos = pos;
            } in
            advance p 1, Some case_ast
        | Token.Colon -> advance (add_error_whitespace (advance p @@ -1) Token.Colon) 1, None
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p Token.Colon) 1, None in

      ___case p in

    let __default p =
      let tok1, tok2 = peek p 0, peek p 1 in
      match tok1.kind, tok2.kind with
      | Token.Identifier "default", Token.Colon when not tok1.has_whitespace_after ->
          let default_ast : Ast.t = {
            kind = Ast.Default;
            pos = pos;
          } in
          advance p 2, Some default_ast
      | Token.Identifier "default", Token.Colon ->
          advance (add_error_whitespace p Token.Colon) 1, None
      | Token.Identifier "default", Token.Eof -> add_error_eof (advance p 1), None
      | Token.Identifier "default", _ ->
          advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
      | Token.Identifier _, _ -> advance (add_error_value p "default") 1, None
      | Token.Eof, _ -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p @@ Token.Identifier "default") 1, None in

    match (peek p 0).kind with
    | Token.Identifier value ->
        let p, case = match value with
                      | "default" -> __default p
                      | "case" -> __case p
                      | _ -> advance (add_error_value_s p ["case"; "default"]) 1, None in
        (match case with
        | None -> p, None
        | Some x ->
            let p, rest = _cases p in
            match rest with
            | None -> p, None
            | Some y -> p, Some (x :: y))
    | Token.RCBrack
    | Token.LCBrack -> p, Some []
    | _ -> advance (add_error_unexpected_s p [ Token.Identifier "case"
                                             ; Token.Identifier "default" ]) 1, None in

  let p, cases = _cases p in
  match cases with
  | None -> p, None
  | Some x ->
      let p, body, success =
        match (peek p 0).kind with
        | Token.LCBrack ->
            let p, block = parse_stmt_block p in
            (match block with
            | None -> p, None, false
            | Some _ -> p, block, true)
        | _ -> p, None, true in
      if not success then p, None
      else
        let case_chain_ast : Ast.t = {
          kind = Ast.CaseChain { cases = x; body = body };
          pos = pos;
        } in
        p, Some case_chain_ast
