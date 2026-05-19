open Parser_core
open Parser_stmt
open Parser_attributes
open Parser_expr

let parse_foreach p =
  let pos = p.pos in

  (* foreach ... ( name : expr ) stmt *)
  (* ^~~~~~~                          *)
  let rec _foreach p =
    match (peek p 0).kind with
    | Token.Identifier "foreach" -> advance p 1 |> _attribs
    | Token.Identifier _ -> advance (add_error_value p "foreach") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "foreach") 1, None

  (* foreach [[attribs...]] ( name : expr ) stmt *)
  (*         ^~~~~~~~~~~~~~                      *)
  and _attribs p =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _lparen p attribs)
    | _ -> _lparen p None

  (* foreach ... ( name : expr ) stmt *)
  (*             ^                    *)
  and _lparen p attribs_opt =
    match (peek p 0).kind with
    | Token.LParen -> _name (advance p 1) attribs_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* foreach ... ( name : expr ) stmt *)
  (*               ^~~~               *)
  and _name p attribs_opt =
    match (peek p 0).kind with
    | Token.Identifier value -> _colon (advance p 1) attribs_opt value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* foreach ... ( name : expr ) stmt *)
  (*                    ^             *)
  and _colon p attribs_opt name =
    match (peek p 0).kind with
    | Token.Colon -> _expr (advance p 1) attribs_opt name
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Colon) 1, None

  (* foreach ... ( name : expr ) stmt *)
  (*                      ^~~~        *)
  and _expr p attribs_opt name =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p attribs_opt name x

  (* foreach ... ( name : expr ) stmt *)
  (*                           ^      *)
  and _rparen p attribs_opt name expr =
    match (peek p 0).kind with
    | Token.RParen -> _body (advance p 1) attribs_opt name expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* foreach ... ( name : expr ) stmt *)
  (*                             ^~~~ *)
  and _body p attribs_opt name expr =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some x ->
        let foreach_ast : Ast.t = {
          kind = Ast.StmtForeach { item_name = name
                                 ; iteratable = expr
                                 ; body = x
                                 ; attribute_list = attribs_opt };
          pos = pos;
        } in
        p, Some foreach_ast in

  _foreach p
