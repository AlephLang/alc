open Parser_core
open Parser_stmt
open Parser_expr
open Parser_attributes

let parse_dowhile p =
  let pos = p.pos in

  (* do stmt while ... (expr); *)
  (* ^~                        *)
  let rec _do p =
    match (peek p 0).kind with
    | Token.Identifier "do" -> advance p 1 |> _body
    | Token.Identifier _ -> advance (add_error_value p "do") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "do") 1, None

  (* do stmt while ... (expr); *)
  (*    ^~~~                   *)
  and _body p =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some x -> _while p x

  (* do stmt while ... (expr); *)
  (*         ^~~~~             *)
  and _while p body =
    match (peek p 0).kind with
    | Token.Identifier "while" -> _attribs (advance p 1) body
    | Token.Identifier _ -> advance (add_error_value p "while") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "while") 1, None

  (* do stmt while [[attribs...]] (expr); *)
  (*               ^~~~~~~~~~~~~~         *)
  and _attribs p body =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some x -> _lparen p body attribs)
    | _ -> _lparen p body None

  (* do stmt while ... (expr); *)
  (*                   ^       *)
  and _lparen p body attribs_opt =
    match (peek p 0).kind with
    | Token.LParen -> _cond (advance p 1) body attribs_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* do stmt while ... (expr); *)
  (*                    ^~~~   *)
  and _cond p body attribs_opt =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p body attribs_opt x

  (* do stmt while ... (expr); *)
  (*                        ^  *)
  and _rparen p body attribs_opt cond =
    match (peek p 0).kind with
    | Token.RParen -> _semicolon (advance p 1) body attribs_opt cond
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* do stmt while ... (expr); *)
  (*                         ^ *)
  and _semicolon p body attribs_opt cond =
    match (peek p 0).kind with
    | Token.Semicolon ->
        let dowhile_stmt : Ast.t = {
          kind = Ast.StmtDoWhile { condition = cond; body = body; attribute_list = attribs_opt };
          pos = pos;
        } in
        advance p 1, Some dowhile_stmt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None in

  _do p
