open Parser_core
open Parser_expr
open Parser_attributes

let parse_enum p =
  let pos = p.pos + 1 in

  (* enum name ... { elements... } *)
  (* ^~~~                          *)
  let rec _enum p =
    match (peek p 0).kind with
    | Token.Identifier "enum" -> advance p 1 |> _name
    | Token.Identifier _ -> advance (add_error_value p "enum") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "enum") 1, None

  (* enum name ... { elements... } *)
  (*      ^~~~                     *)
  and _name p =
    match (peek p 0).kind with
    | Token.Identifier value -> _attribs (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* enum name [[attribs...]] { elements... } *)
  (*           ^~~~~~~~~~~~~~                 *)
  and _attribs p name =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _elements p name attribs)
    | _ -> _elements p name None

  (* enum name [[attribs...]] { elements... } *)
  (*           ^~~~~~~~~~~~~~                 *)
  and _elements p name attribs_opt =
    match (peek p 0).kind with
    | Token.LCBrack ->
        let p, elements = _get_elements (advance p 1) in
        (match elements with
        | None -> p, None
        | Some x ->
            let enum_ast : Ast.t = {
              kind = Ast.Enum { name = name
                              ; elements = x
                              ; attribute_list = attribs_opt };
              pos = pos;
            } in
            p, Some enum_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LCBrack) 1, None

  and _get_elements p =
    match (peek p 0).kind with
    | Token.RCBrack -> advance p 1, Some []
    | Token.Identifier value ->
        let pos = p.pos in
        let p = advance p 1 in
        let p, expr, success =
          match (peek p 0).kind with
          | Token.Eq ->
              let p, expr = parse_expr (advance p 1) false in
              (match expr with
              | None -> p, None, false
              | Some _ -> p, expr, true)
          | _ -> p, None, true in
        if not success then p, None
        else
          let p, rest =
            match (peek p 0).kind with
            | Token.RCBrack -> advance p 1, Some []
            | Token.Comma -> _get_elements (advance p 1)
            | Token.Eof -> add_error_eof p, None
            | _ -> advance (add_error_unexpected_s p [Token.RCBrack; Token.Comma]) 1, None in
          (match rest with
          | None -> p, None
          | Some x ->
              let enum_element_ast : Ast.t = {
                kind = Ast.EnumElement { name = value; expression = expr };
                pos = pos;
              } in
              p, Some (enum_element_ast :: x))
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected_s p [Token.RCBrack; Token.Identifier ""]) 1, None in

  _enum p
