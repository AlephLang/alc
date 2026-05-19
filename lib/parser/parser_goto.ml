open Parser_core

let parse_goto p =
  let pos = p.pos in

  (* goto @name ; *)
  (* ^~~~         *)
  let rec _goto p =
    match (peek p 0).kind with
    | Token.Identifier "goto" -> advance p 1 |> _name
    | Token.Identifier _ -> advance (add_error_value p "goto") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "goto") 1, None

  (* goto @name ; *)
  (*      ^~~~~   *)
  and _name p =
    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.At, Token.Identifier value when not tok1.has_whitespace_after ->
        _semicolon (advance p 2) value
    | Token.At, Token.Identifier _ ->
        advance (add_error_whitespace p @@ Token.Identifier "label name") 1, None
    | Token.At, Token.Eof -> add_error_eof (advance p 1), None
    | Token.At, _ -> advance (add_error_unexpected (advance p 1) @@ Token.Identifier "") 1, None
    | Token.Eof, _ -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.At) 1, None

  (* goto @name ; *)
  (*            ^ *)
  and _semicolon p name =
    match (peek p 0).kind with
    | Token.Semicolon ->
        let goto_ast : Ast.t = {
          kind = Ast.StmtGoto { label_name = name };
          pos = pos;
        } in
        advance p 1, Some goto_ast
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None in

  _goto p
