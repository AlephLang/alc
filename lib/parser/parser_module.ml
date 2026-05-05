open Parser_core

let rec try_parse_submodule p =
  let tok1, tok2 = peek p 0, peek p 1 in
  match tok1.kind, tok2.kind with
  | Token.Colon, Token.Colon when not tok1.has_whitespace_after -> advance p 2 |> parse_module
  | _, _ -> p, None

and parse_module p =
  let pos = p.pos in
  let tok = peek p 0 in
  match tok.kind with
  | Token.Identifier value ->
      let p = advance p 1 in
      let p, subm = try_parse_submodule p in
      let module_ast : Ast.t = {
        kind = Ast.Module { name = value; subm = subm };
        pos = pos;
      } in
      p, Some module_ast
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None
