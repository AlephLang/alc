open Parser_core
open Parser_type

let parse_vardecl p attribs =
  let pos = p.pos in

  (* name: type *)
  (* ^~~~       *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier value -> _colon_and_type (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* name: type *)
  (*     ^ ^~~~ *)
  and _colon_and_type p name =
    match (peek p 0).kind with
    | Token.Colon ->
        let p, type_ = advance p 1 |> parse_type in
        (match type_ with
        | None -> p, None
        | Some x ->
            let vardecl_ast : Ast.t = {
              kind = Ast.VarDecl { name = name; type_ = x; attribute_list = attribs };
              pos = pos;
            } in
            p, Some vardecl_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Colon) 1, None in

  _id p
