open Parser_core
open Parser_stmt
open Parser_expr
open Parser_attributes

let parse_for p =
  let pos = p.pos in

  (* for ... ( init-stmt cond ; expr ) stmt *)
  (* ^~~                                    *)
  let rec _for p =
    match (peek p 0).kind with
    | Token.Identifier "for" -> advance p 1 |> _attribs
    | Token.Identifier _ -> advance (add_error_value p "for") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "for") 1, None

  (* for [[attribs...]] ( init-stmt cond ; expr ) stmt *)
  (*     ^~~~~~~~~~~~~~                                *)
  and _attribs p =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _lparen p attribs)
    | _ -> _lparen p None

  (* for ... ( init-stmt cond ; expr ) stmt *)
  (*         ^                              *)
  and _lparen p attribs_opt =
    match (peek p 0).kind with
    | Token.LParen -> _init_stmt (advance p 1) attribs_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* for ( init-stmt cond ; expr ) stmt *)
  (*       ^~~~~~~~~                    *)
  and _init_stmt p attribs_opt =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some _ -> _cond p attribs_opt stmt

  (* for ( init-stmt cond ; expr ) stmt *)
  (*                 ^~~~~~             *)
  and _cond p attribs_opt init_stmt_opt =
    let p, cond_expr, success =
      match (peek p 0).kind with
      | Token.Semicolon -> p, None, true
      | _ ->
          let p, cond_expr = parse_expr p false in
          match cond_expr with
          | None -> p, None, false
          | Some _ -> p, cond_expr, true in
    if not success then p, None
    else
      match (peek p 0).kind with
      | Token.Semicolon -> _expr (advance p 1) attribs_opt init_stmt_opt cond_expr
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None

  (* for ( init-stmt cond ; expr ) stmt *)
  (*                        ^~~~        *)
  and _expr p attribs_opt init_stmt_opt cond_opt =
    match (peek p 0).kind with
    | Token.RParen -> _rparen p attribs_opt init_stmt_opt cond_opt None
    | _ ->
        let p, toplevel_expr = parse_expr p true in
        match toplevel_expr with
        | None -> p, None
        | Some _ -> _rparen p attribs_opt init_stmt_opt cond_opt toplevel_expr

  (* for ( init-stmt cond ; expr ) stmt *)
  (*                             ^      *)
  and _rparen p attribs_opt init_stmt_opt cond_opt expr_opt =
    match (peek p 0).kind with
    | Token.RParen -> _body (advance p 1) attribs_opt init_stmt_opt cond_opt expr_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  and _body p attribs_opt init_stmt_opt cond_opt expr_opt =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some x ->
        let for_ast : Ast.t = {
          kind = Ast.StmtFor { init_statement = init_stmt_opt
                             ; condition = cond_opt
                             ; expression = expr_opt
                             ; body = x
                             ; attribute_list = attribs_opt };
          pos = pos;
        } in
        p, Some for_ast in

  _for p
