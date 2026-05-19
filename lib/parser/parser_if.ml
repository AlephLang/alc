open Parser_core
open Parser_attributes
open Parser_expr
open Parser_stmt

let parse_if p =
  let pos = p.pos in

  (* if ... ( expr ) stmt ... *)
  (* ^~                       *)
  let rec _if p =
    match (peek p 0).kind with
    | Token.Identifier "if" -> advance p 1 |> _attribs
    | Token.Identifier _ -> advance (add_error_value p "if") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "if") 1, None

  (* if [[attribs...]] ( expr ) stmt ... *)
  (*    ^~~~~~~~~~~~~~                   *)
  and _attribs p =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _lparen p attribs)
    | _ -> _lparen p None

  (* if ... ( expr ) stmt ... *)
  (*        ^                 *)
  and _lparen p attribs_opt =
    match (peek p 0).kind with
    | Token.LParen -> _cond (advance p 1) attribs_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* if ... ( expr ) stmt ... *)
  (*          ^~~~            *)
  and _cond p attribs_opt =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p attribs_opt x

  (* if ... ( expr ) stmt ... *)
  (*               ^          *)
  and _rparen p attribs_opt cond =
    match (peek p 0).kind with
    | Token.RParen -> _body (advance p 1) attribs_opt cond
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* if ... ( expr ) stmt ... *)
  (*                 ^~~~     *)
  and _body p attribs_opt cond =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some x ->
        let p, else_, success =
          match (peek p 0).kind with
          | Token.Identifier "else" -> let p, else_ = _else p in p, else_, Option.is_some else_
          | _ -> p, None, true in
        if not success then p, None
        else
          let if_ast : Ast.t = {
            kind = Ast.StmtIf { condition = cond
                              ; body = x
                              ; else_statement = else_
                              ; attribute_list = attribs_opt };
            pos = pos;
          } in
          p, Some if_ast

  (* if ... ( expr ) stmt else stmt *)
  (*                                *)
  and _else p =
    let pos = p.pos in

    (* else stmt *)
    (* ^~~~      *)
    let rec __else p =
      match (peek p 0).kind with
      | Token.Identifier "else" -> advance p 1 |> __stmt
      | Token.Identifier _ -> advance (add_error_value p "else") 1, None
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p @@ Token.Identifier "else") 1, None

    (* else stmt *)
    (*      ^~~~ *)
    and __stmt p =
      let p, stmt = parse_stmt p in
      match stmt with
      | None -> p, None
      | Some x ->
          let else_ast : Ast.t = {
            kind = Ast.StmtElse { body = x };
            pos = pos;
          } in
          p, Some else_ast in

    __else p in

  _if p
