open Parser_core
open Parser_type
open Parser_function_arguments

let parse_extern p =
  let pos = p.pos in

  (* extern name ... ; *)
  (* ^~~~~~            *)
  let rec _extern p =
    match (peek p 0).kind with
    | Token.Identifier "extern" -> advance p 1 |> _name
    | Token.Identifier _ -> advance (add_error_value p "extern") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "extern") 1, None

  (* extern name ... ; *)
  (*        ^~~~       *)
  and _name p =
    match (peek p 0).kind with
    | Token.Identifier value -> _split (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* extern name : type ; *) (* extern name :: (args...) ... ; *)
  (*             ^        *) (*             ^~                 *)
  and _split p name =
    (* extern name :: (args...) ... ; *)
    let __func p =
      (* extern name :: (args...) ... ; *)
      (*                ^~~~~~~~~       *)
      let rec ___arguments p =
        let p, arguments = parse_function_arguments p in
        match arguments with
        | None -> p, None
        | Some x ->
            match (peek p 0).kind with
            | Token.Minus -> ___type p x
            | _ -> ___semicolon p x None

      (* extern name :: (args...) -> type ; *)
      (*                             ^~~~   *)
      and ___type p arguments =
        let tok1, tok2 = peek p 0, peek p 1 in
        match tok1.kind, tok2.kind with
        | Token.Minus, Token.RArrow when not tok1.has_whitespace_after ->
            let p, type_ = advance p 2 |> parse_type in
            (match type_ with
            | None -> p, None
            | Some _ -> ___semicolon p arguments type_)
        | Token.Minus, Token.RArrow -> advance (add_error_whitespace p Token.RArrow) 1, None
        | Token.Minus, Token.Eof -> add_error_eof (advance p 1), None
        | Token.Minus, _ -> advance (add_error_unexpected (advance p 1) Token.RArrow) 1, None
        | Token.Eof, _ -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p Token.Minus) 1, None

      (* extern name :: (args...) -> type ; *)
      (*                                  ^ *)
      and ___semicolon p arguments type_opt =
        match (peek p 0).kind with
        | Token.Semicolon ->
            let extern_func_ast : Ast.t = {
              kind = Ast.ExternFunc { name = name
                                    ; argument_list = arguments
                                    ; return_type = type_opt };
              pos = pos;
            } in
            advance p 1, Some extern_func_ast
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None in

      ___arguments p in

    (* extern name : type ; *)
    let __vardecl p =
      (* extern name : type ; *)
      (*               ^~~~   *)
      let rec ___type p =
        let p, type_ = parse_type p in
        match type_ with
        | None -> p, None
        | Some x -> ___semicolon p x

      (* extern name : type ; *)
      (*                    ^ *)
      and ___semicolon p type_ =
        match (peek p 0).kind with
        | Token.Semicolon ->
            let extern_vardecl_ast : Ast.t = {
              kind = Ast.ExternVarDecl { name = name; type_ = type_ };
              pos = pos;
            } in
            advance p 1, Some extern_vardecl_ast
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None in

      ___type p in

    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.Colon, Token.Colon when not tok1.has_whitespace_after -> advance p 2 |> __func
    | Token.Colon, Token.Colon -> advance (add_error_whitespace p Token.Colon) 1, None
    | Token.Colon, _ -> advance p 1 |> __vardecl
    | Token.Eof, _ -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Colon) 1, None in

  _extern p
