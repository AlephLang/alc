open Parser_core

let parse_label p =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.At, Token.Identifier value, Token.Colon
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after ->
        let label_ast : Ast.t = {
          kind = Ast.StmtLabel { name = value };
          pos = p.pos;
        } in
        advance p 3, Some label_ast
  | Token.At, Token.Identifier _, Token.Colon
    when not tok1.has_whitespace_after ->
      advance (add_error_whitespace (advance p 1) Token.Colon) 1, None
  | Token.At, Token.Identifier _, Token.Colon ->
      advance (add_error_whitespace p Token.Colon) 1, None
  | Token.At, Token.Identifier _, Token.Eof -> add_error_eof (advance p 2), None
  | Token.At, Token.Identifier _, _ ->
      advance (add_error_unexpected (advance p 2) Token.Colon) 1, None
  | Token.At, Token.Eof, _ -> add_error_eof (advance p 1), None
  | Token.At, _, _ -> advance (add_error_unexpected (advance p 1) @@ Token.Identifier "") 1, None
  | Token.Eof, _, _ -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.At) 1, None
