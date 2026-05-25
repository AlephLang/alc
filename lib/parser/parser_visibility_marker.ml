open Parser_core
open Parser_misc

let parse_visibility_marker p =
  let tok1, tok2 = peek p 0, peek p 1 in
  match tok1.kind, tok2.kind with
  | Token.Identifier value, Token.Colon
    when not tok1.has_whitespace_after
          && is_visibility_marker value ->
      let visibility_marker_ast : Ast.t = {
        kind = Ast.VisibilityMarker { name = value };
        pos = p.pos;
      } in
      advance p 2, Some visibility_marker_ast
  | Token.Identifier _, Token.Colon
    when not tok1.has_whitespace_after ->
      advance (add_error_value_s p ["public"; "private"]) 1, None
  | Token.Identifier _, Token.Colon ->
      advance (add_error_whitespace p Token.Colon) 1, None
  | Token.Identifier _, Token.Eof -> add_error_eof (advance p 1), None
  | Token.Identifier _, _ -> advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
  | Token.Eof, _ -> add_error_eof p, None
  | _ -> advance (add_error_unexpected_s p [ Token.Identifier "public"
                                           ; Token.Identifier "private" ]) 1, None
