open Parser_core

let rec try_parse_submodule p =
  match peek p 0, peek p 1 with
  | Some i, Some k ->
      (match i.kind, k.kind with
      | Token.Colon, Token.Colon
        when not i.has_whitespace_after && not k.has_whitespace_after ->
            advance p 2 |> parse_module
      | _, _ -> p, None)
  | _, _ -> p, None

and parse_module p =
  let pos = p.pos in
  match peek p 0 with
  | None -> add_error_eof p, None
  | Some x ->
      match x.kind with
      | Token.Identifier value ->
          let p = advance p 1 in
          let p, subm = try_parse_submodule p in
          let module_ast : Ast.t = {
            kind = Ast.Module { name = value; subm = subm };
            pos = pos;
          } in
          p, Some module_ast
      | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None
