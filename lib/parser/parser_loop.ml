open Parser_core
open Parser_stmt

let parse_loop p =
  let pos = p.pos in

  (* loop stmt *)
  (* ^~~~      *)
  let rec _loop p =
    match (peek p 0).kind with
    | Token.Identifier "loop" -> advance p 1 |> _stmt
    | Token.Identifier _ -> advance (add_error_value p "loop") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "loop") 1, None

  (* loop stmt *)
  (*      ^~~~ *)
  and _stmt p =
    let p, stmt = parse_stmt p in
    match stmt with
    | None -> p, None
    | Some x ->
        let loop_ast : Ast.t = {
          kind = Ast.StmtLoop { body = x };
          pos = pos;
        } in
        p, Some loop_ast in

  _loop p
