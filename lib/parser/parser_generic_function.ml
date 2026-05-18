open Parser_core
open Parser_function_arguments
open Parser_generic_placeholder_type_list
open Parser_type
open Parser_stmt
open Parser_expr

let parse_generic_function p attribs function_type =
  let pos = p.pos in

  (* name :: <types...> (args...) ... *)
  (* ^~~~                             *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier value -> _double_colon (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* name :: <types...> (args...) ... *)
  (*      ^~                          *)
  and _double_colon p name =
    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.Colon, Token.Colon when not tok1.has_whitespace_after ->
        _generic_placeholder_type_list (advance p 2) name
    | Token.Colon, Token.Colon -> advance (add_error_whitespace p Token.Colon) 1, None
    | Token.Colon, Token.Eof -> add_error_eof (advance p 1), None
    | Token.Colon, _ -> advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
    | Token.Eof, _ -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Colon) 1, None

  (* name :: <types...> (args...) ... *)
  (*         ^~~~~~~~~~               *)
  and _generic_placeholder_type_list p name =
    let p, generic_placeholder_type_list = parse_generic_placeholder_type_list p in
    match generic_placeholder_type_list with
    | None -> p, None
    | Some x -> _arguments p name x

  (* name :: <types...> (args...) ... *)
  (*                    ^~~~~~~~~     *)
  and _arguments p name generic_placeholder_type_list =
    let p, arguments = parse_function_arguments p in
    match arguments with
    | None -> p, None
    | Some x ->
        match (peek p 0).kind with
        | Token.Minus -> _type p name generic_placeholder_type_list x
        | _ -> _body p name generic_placeholder_type_list x None

  (* name :: <types...> (args...) -> type ... *)
  (*                              ^~ ^~~~     *)
  and _type p name generic_placeholder_type_list arguments =
    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.Minus, Token.RArrow when not tok1.has_whitespace_after ->
        let p, type_ = advance p 2 |> parse_type in
        (match type_ with
        | None -> p, None
        | Some _ -> _body p name generic_placeholder_type_list arguments type_)
    | Token.Minus, Token.RArrow -> advance (add_error_whitespace p Token.RArrow) 1, None
    | Token.Minus, Token.Eof -> add_error_eof (advance p 1), None
    | Token.Minus, _ -> advance (add_error_unexpected (advance p 1) Token.RArrow) 1, None
    | Token.Eof, _ -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Minus) 1, None

  (* name :: <types...> (args...) ... { ... } *) (* name :: <types...> (args...) ... $ expr; *)
  (*                                          *) (*                                  ^ ^~~~^ *)
  and _body p name generic_placeholder_type_list arguments type_opt =
    let p, body =
      let tok = peek p 0 in
      match tok.kind with
      | Token.LCBrack -> parse_stmt_block p
      | Token.Identifier value when value = "$" && tok.has_whitespace_after ->
          let p, expr = parse_expr (advance p 1) false in
          (match expr with
          | None -> p, None
          | Some _ ->
              match (peek p 0).kind with
              | Token.Semicolon -> advance p 1, expr
              | Token.Eof -> add_error_eof p, None
              | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None)
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected_s p [Token.LCBrack; Token.Identifier "$"]) 1, None in
    match body with
    | None -> p, None
    | Some x ->
        let generic_function_ast : Ast.t = {
          kind = Ast.GenericFunc { name = name
                                 ; function_type = function_type
                                 ; generic_placeholder_type_list = generic_placeholder_type_list
                                 ; argument_list = arguments
                                 ; return_type = type_opt
                                 ; body = x
                                 ; attribute_list = attribs };
          pos = pos;
        } in
        p, Some generic_function_ast in

  _id p
