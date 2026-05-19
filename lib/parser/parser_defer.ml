open Parser_core

let __parse_stmt : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)

let parse_defer p =
  match (peek p 0).kind with
  | Token.Identifier "defer" ->
      let pos = p.pos in
      let p, stmt = advance p 1 |> !__parse_stmt in
      (match stmt with
      | None -> p, None
      | Some x ->
        let defer_stmt : Ast.t = {
          kind = Ast.StmtDefer { body = x };
          pos = pos;
        } in
        p, Some defer_stmt)
  | Token.Identifier _ -> advance (add_error_value p "defer") 1, None
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "defer") 1, None
