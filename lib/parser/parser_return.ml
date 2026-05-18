open Parser_core
open Parser_initlist
open Parser_expr

let parse_return p =
  let pos = p.pos in

  (* return [expr, initlist] ; *)
  (*                           *)
  let rec _return p =
    match (peek p 0).kind with
    | Token.Identifier value when value = "return" -> advance p 1 |> _expr
    | Token.Identifier _ -> advance (add_error_value p "return") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "return") 1, None

  (* return [expr, initlist] ; *)
  (*        ^~~~~~~~~~~~~~~~   *)
  and _expr p =
    match (peek p 0).kind with
    | Token.Semicolon -> _semicolon (advance p 1) None
    | _ ->
        let p, value =
          match (peek p 0).kind with
          | Token.LCBrack -> parse_initlist p
          | _ -> parse_expr p false in
        match value with
        | None -> p, None
        | Some x -> _semicolon p value

  (* return [expr, initlist] ; *)
  (*                         ^ *)
  and _semicolon p value_opt =
    match (peek p 0).kind with
    | Token.Semicolon ->
        let return_ast : Ast.t = {
          kind = Ast.StmtReturn { expression = value_opt };
          pos = pos;
        } in
        advance p 1, Some return_ast
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None in

  _return p
