open Parser_core
open Parser_import

let parse_top p =
  match peek p 0 with
  | None -> add_error_eof p, None
  | Some x ->
      match x.kind with
      | Token.Semicolon ->
          let none_ast : Ast.t = {
            kind = Ast.None;
            pos = p.pos;
          } in
          advance p 1, Some none_ast
      | Token.Identifier value ->
          (match value with
          | "import" -> parse_import p
          | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None)
      | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None
