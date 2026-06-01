open Parser_core
open Parser_decldef
open Parser_attributes
open Parser_struct
open Parser_enum
open Parser_typedef

let rec parse_union p =
  let pos = p.pos + 1 in

  (* union name ... { ... } *)
  (* ^~~~~                  *)
  let rec _union p =
    match (peek p 0).kind with
    | Token.Identifier "union" -> advance p 1 |> _name
    | Token.Identifier _ -> advance (add_error_value p "union") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "union") 1, None

  (* union name ... { ... } *)
  (*       ^~~~             *)
  and _name p =
    match (peek p 0).kind with
    | Token.Identifier value -> _attribs (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* union name [[attribs]] { ... } *)
  (*            ^~~~~~~~~~~         *)
  and _attribs p name =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _children p name attribs)
    | _ -> _children p name None

  (* union name [[attribs]] { ... } *)
  (*                        ^~~~~~~ *)
  and _children p name attribs_opt =
    match (peek p 0).kind with
    | Token.LCBrack ->
        let p, children = advance p 1 |> _get_children in
        (match children with
        | None -> p, None
        | Some x ->
            let union_ast : Ast.t = {
              kind = Ast.Union { name = name; children = x; attribute_list = attribs_opt };
              pos = pos;
            } in
            p, Some union_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LCBrack) 1, None

  and _get_children p =
    match (peek p 0).kind with
    | Token.RCBrack -> advance p 1, Some []
    | _ ->
      let p, cur =
        match (peek p 0).kind with
        | Token.Identifier "struct" -> parse_struct p
        | Token.Identifier "enum" -> parse_enum p
        | Token.Identifier "union" -> parse_union p
        | Token.Identifier "using" -> parse_typedef p
        | Token.LBrack ->
            let p, attribs = parse_attribute_list p in
            (match attribs with
            | None -> p, None
            | Some _ -> parse_decldef p attribs)
        | _ -> parse_decldef p None in
      match cur with
      | None -> p, None
      | Some x ->
          let p, rest = _get_children p in
          match rest with
          | None -> p, None
          | Some y -> p, Some (x :: y) in

  _union p
