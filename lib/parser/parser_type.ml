open Parser_core
open Parser_function_arguments
open Parser_generic_type_list
open Parser_expr

type ptrOrRef = Ptr | Ref

let rec gather_pointers_and_references p =
  let ptr_or_ref_opt =
    match (peek p 0).kind with
    | Token.Asterisk -> Some Ptr
    | Token.Ampersand -> Some Ref
    | _ -> None in
  match ptr_or_ref_opt with
  | None -> p, []
  | Some x ->
      let p, rest = advance p 1 |> gather_pointers_and_references in
      p, rest @ [x]

let rec apply_pointers_and_references raw_pos ptr_ref_list ast =
  match ptr_ref_list with
  | [] -> ast
  | x :: xs ->
      let kind = match x with
                 | Ptr -> Ast.TypePointer { type_ = ast }
                 | Ref -> Ast.TypeReference { type_ = ast } in
      let ast : Ast.t = {
        kind = kind;
        pos = raw_pos + List.length ptr_ref_list - 1;
      } in
      apply_pointers_and_references raw_pos xs ast

let rec parse_type_raw p =
  match (peek p 0).kind with
  | Token.Identifier "typeof" -> parse_typeof p
  | Token.Identifier value ->
      (match (peek p 1).kind with
      | Token.ExclMark -> parse_generic_type_or_namespace p
      | Token.Colon -> parse_namespace p
      | _ ->
        let ast_type_plain : Ast.t = {
          kind = Ast.TypePlain { name = value };
          pos = p.pos;
        } in
        advance p 1, Some ast_type_plain)
  | Token.LParen -> parse_function_pointer p
  | _ -> p, None

and parse_type_array p ast =
  match (peek p 0).kind with
  | Token.LBrack ->
      let pos = p.pos in
      let p = advance p 1 in
      let p, size_expression, success = 
        match (peek p 0).kind with
        | Token.RBrack -> p, None, true
        | _ ->
            let p, expr = parse_expr p false in
            match expr with
            | None -> p, None, false
            | Some x -> p, expr, true in
      if not success then p, None, false
      else
        (match (peek p 0).kind with
        | Token.RBrack ->
            let arr : Ast.t = {
              kind = Ast.TypeArray {
                type_ = ast;
                size_expression = size_expression;
              };
              pos = pos;
            } in
            parse_type_array (advance p 1) arr
        | Token.Eof -> add_error_eof p, None, false
        | _ -> advance (add_error_unexpected p @@ Token.RBrack) 1, None, false)
  | _ -> p, Some ast, true

and parse_function_pointer p =
  let pos = p.pos in

  (* (args...) ... *)
  (* ^~~~~~~~~     *)
  let rec _arguments p =
    match (peek p 0).kind with
    | Token.LParen ->
        let p, args = parse_function_arguments p in
        (match args with
        | None -> p, None
        | Some x -> _type p x)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  and _type p arguments =
    let p, return_type, success =
      let tok1, tok2 = peek p 0, peek p 1 in
      match tok1.kind, tok2.kind with
      | Token.Minus, Token.RArrow when not tok1.has_whitespace_after ->
          let p, type_ = advance p 2 |> parse_type in
          (match type_ with
          | None -> p, None, false
          | Some _ -> p, type_, true)
      | Token.Minus, Token.RArrow -> advance (add_error_whitespace p Token.RArrow) 1, None, false
      | Token.Minus, Token.Eof -> add_error_eof (advance p 1), None, false
      | Token.Minus, _ -> advance (add_error_unexpected (advance p 1) Token.RArrow) 1, None, false
      | Token.Eof, _ -> add_error_eof p, None, false
      | _ -> p, None, true in
    if not success then p, None
    else
      let type_funcptr_ast : Ast.t = {
        kind = Ast.TypeFunctionPointer { argument_list = arguments; return_type = return_type };
        pos = pos;
      } in
      p, Some type_funcptr_ast in

  _arguments p

and parse_generic_type_or_namespace p =
  let pos = p.pos in

  (* identifier!<types...> ... *)
  (* ^~~~~~~~~~                *)
  let rec _id p =
    let tok1 = peek p 0 in
    match tok1.kind with
    | Token.Identifier value when not tok1.has_whitespace_after ->
        _generic_type_list (advance p 1) value
    | Token.Identifier _ -> advance (add_error_whitespace p Token.LArrow) 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* identifier!<types...> ... *)
  (*           ^~~~~~~~~~~     *)
  and _generic_type_list p name =
    let p, generic_type_list = parse_generic_type_list p in
    match generic_type_list with
    | None -> p, None
    | Some x -> _type_or_namespace p name x

  and _type_or_namespace p name generic_type_list =
    let tok0, tok1, tok2 = peek p @@ -1, peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.Colon, Token.Colon
      when not tok0.has_whitespace_after
        && not tok1.has_whitespace_after
        && not tok2.has_whitespace_after ->
          let p, subobject = advance p 2 |> parse_type_raw in
          (match subobject with
          | None -> p, None
          | Some x ->
              let generic_namespace_ast : Ast.t = {
                kind = Ast.GenericNamespace { name = name
                                            ; generic_type_list = generic_type_list
                                            ; subobject = x };
                pos = pos;
              } in
              p, Some generic_namespace_ast)
    | Token.Colon, Token.Colon
      when not tok0.has_whitespace_after
        && not tok1.has_whitespace_after ->
          advance (add_error_whitespace (advance p 1) @@ Token.Identifier "") 1, None
    | Token.Colon, Token.Colon
      when not tok0.has_whitespace_after ->
          advance (add_error_whitespace p @@ Token.Identifier "") 1, None
    | Token.Colon, Token.Colon ->
          advance (add_error_whitespace (advance p @@ -1) @@ Token.Identifier "") 1, None
    | Token.Colon, Token.Eof -> add_error_eof (advance p 1), None
    | Token.Colon, _ -> advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
    | _ ->
        let generic_type_ast : Ast.t = {
          kind = Ast.GenericType { name = name; generic_type_list = generic_type_list };
          pos = pos;
        } in
        p, Some generic_type_ast in

  _id p

and parse_namespace p =
  let pos = p.pos in
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.Identifier value, Token.Colon, Token.Colon
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after
      && not tok3.has_whitespace_after ->
        let p, subobject = advance p 3 |> parse_type_raw in
        (match subobject with 
        | None -> p, None
        | Some x ->
            let namespace_ast : Ast.t = {
              kind = Ast.Namespace { name = value; subobject = x };
              pos = pos;
            } in
            p, Some namespace_ast)
  | Token.Identifier value, Token.Colon, Token.Colon
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after ->
        advance (add_error_whitespace (advance p 2) Token.Colon) 1, None
  | Token.Identifier value, Token.Colon, Token.Colon
    when not tok1.has_whitespace_after ->
        advance (add_error_whitespace (advance p 1) Token.Colon) 1, None
  | Token.Identifier value, Token.Colon, Token.Colon ->
        advance (add_error_whitespace p Token.Colon) 1, None
  | Token.Identifier value, Token.Colon, Token.Eof -> add_error_eof (advance p 2), None
  | Token.Identifier value, Token.Colon, _ ->
      advance (add_error_unexpected (advance p 2) Token.Colon) 1, None
  | Token.Identifier value, Token.Eof, _ -> add_error_eof (advance p 1), None
  | Token.Identifier value, _, _ -> advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
  | Token.Eof, _, _ -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

and parse_typeof p =
  let pos = p.pos in

  (* typeof ( expr ) *)
  (* ^~~~~~          *)
  let rec _typeof p =
    match (peek p 0).kind with
    | Token.Identifier "typeof" -> advance p 1 |> _lparen
    | Token.Identifier _ -> advance (add_error_value p "typeof") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "typeof") 1, None

  (* typeof ( expr ) *)
  (*        ^        *)
  and _lparen p =
    match (peek p 0).kind with
    | Token.LParen -> advance p 1 |> _expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* typeof ( expr ) *)
  (*          ^~~~   *)
  and _expr p =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p x

  (* typeof ( expr ) *)
  (*               ^ *)
  and _rparen p expr =
    match (peek p 0).kind with
    | Token.RParen ->
        let typeof_type_ast : Ast.t = {
          kind = Ast.TypeTypeOf { expression = expr };
          pos = pos;
        } in
        advance p 1, Some typeof_type_ast
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None in

  _typeof p

and parse_type p =
  let pos = p.pos in
  let p, pointers_and_references = gather_pointers_and_references p in
  let p, type_raw = parse_type_raw p in
  match type_raw with
  | None -> p, None
  | Some x ->
      let type_ptr = apply_pointers_and_references pos pointers_and_references x in
      let p, array_type, success = parse_type_array p type_ptr in
      if not success then p, None
      else
        match array_type with
        | Some y -> p, array_type
        | None -> p, None
