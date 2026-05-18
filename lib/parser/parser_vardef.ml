open Parser_core
open Parser_expr
open Parser_type
open Parser_attributes
open Parser_initlist

let parse_vardef p attribs =
  let pos = p.pos in

  (* name: type = value *) (* name := value *)
  (* ^~~~               *) (* ^~~~          *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier value -> _colon (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* name: type = value *) (* name := value *)
  (*     ^              *) (*      ^        *)
  and _colon p name =
    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.Colon, Token.Eq when not tok1.has_whitespace_after ->
        _value (advance p 2) name None
    | Token.Colon, _ ->
        let p, type_ = advance p 1 |> parse_type in
        (match type_ with
        | None -> p, None
        | Some x -> _eq p name x)
    | Token.Eof, _ -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Colon) 1, None

  (* name: type = value *)
  (*            ^       *)
  and _eq p name type_ =
    match (peek p 0).kind with
    | Token.Eq -> _value (advance p 1) name @@ Some type_
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Eq) 1, None

  (* name: type = value *) (* name := value *)
  (*              ^~~~~ *) (*         ^~~~~ *)
  and _value p name type_opt =
    let p, value =
      match (peek p 0).kind with
      | Token.LCBrack -> parse_initlist p
      | _ -> parse_expr p false in
    match value with
    | None -> p, None
    | Some x ->
        let vardef_ast : Ast.t = {
          kind = Ast.VarDef { name = name
                            ; type_ = type_opt
                            ; expression = x
                            ; attribute_list = attribs };
          pos = pos;
        } in
        p, Some vardef_ast in

  _id p
