open Parser_core
open Parser_stmt
open Parser_attributes
open Parser_expr

let parse_while p =
  let pos = p.pos in

  (* while ... (expr) stmt *)
  (* ^~~~~                 *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier "while" -> advance p 1 |> _attrs
    | Token.Identifier _ -> advance (add_error_value p "while") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "while") 1, None

  (* while [attribs...] (expr) stmt *)
  (*       ^~~~~~~~~~~~             *)
  and _attrs p =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some x -> _lparen p attribs)
    | _ -> _lparen p None

  (* while (expr) stmt *)
  (*       ^           *)
  and _lparen p attribs_opt =
    match (peek p 0).kind with
    | Token.LParen -> _cond (advance p 1) attribs_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* while (expr) stmt *)
  (*        ^~~~       *)
  and _cond p attribs_opt =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p attribs_opt x

  (* while (expr) stmt *)
  (*            ^      *)
  and _rparen p attribs_opt expr =
    match (peek p 0).kind with
    | Token.RParen -> _body (advance p 1) attribs_opt expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* while (expr) stmt *)
  (*              ^~~~ *)
  and _body p attribs_opt expr =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some x ->
        let while_stmt : Ast.t = {
          kind = Ast.StmtWhile { condition = expr; body = x; attribute_list = attribs_opt };
          pos = pos;
        } in
        p, Some while_stmt in

  _id p
