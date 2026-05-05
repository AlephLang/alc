open Parser_core
open Parser_module

let parse_import p =
  let pos = p.pos in
  let p, m = advance p 1 |> parse_module in
  match m with
  | None -> p, None
  | Some x ->
      match peek p 0 with
      | None -> add_error_eof p, None
      | Some y ->
          match y.kind with
          | Token.Semicolon ->
              let import_ast : Ast.t = {
                kind = Ast.Import { m = x };
                pos = pos;
              } in
              advance p 1, Some import_ast
          | _ -> advance (add_error_unexpected p @@ Token.Semicolon) 1, None
