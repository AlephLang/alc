open Parser_core
open Parser_stmt
open Parser_attributes
open Parser_decldef
open Parser_enum
open Parser_generic_placeholder_type_list

let __parse_union : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)

let rec parse_struct p =
  (* pos + 1 because we want to get the name of the structure, not the "struct" keyword *)
  let pos = p.pos + 1 in

  (* struct name ... { [field/method/func]s... } *)
  (* ^~~~~~                                      *)
  let rec _struct p =
    match (peek p 0).kind with
    | Token.Identifier "struct" -> advance p 1 |> _name
    | Token.Identifier _ -> advance (add_error_value p "struct") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "struct") 1, None

  (* struct name ... { [field/method/func]s... } *)
  (*        ^~~~                                 *)
  and _name p =
    match (peek p 0).kind with
    | Token.Identifier value -> _generic (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* struct name <types...> ... { [field/method/func]s... } *)
  (*             ^~~~~~~~~~                                 *)
  and _generic p name =
    match (peek p 0).kind with
    | Token.LArrow ->
        let p, generic_placeholder_type_list = parse_generic_placeholder_type_list p in
        (match generic_placeholder_type_list with
        | None -> p, None
        | Some _ -> _attribs p name generic_placeholder_type_list)
    | _ -> _attribs p name None

  (* struct name ... [[attrs...]] { [field/method/func]s... } *)
  (*                 ^~~~~~~~~~~~                             *)
  and _attribs p name gptl_opt =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _children p name gptl_opt attribs)
    | _ -> _children p name gptl_opt None

  (* struct name ... { [field/method/func]s... } *)
  (*                 ^~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
  and _children p name gptl_opt attribs_opt =
    match (peek p 0).kind with
    | Token.LCBrack ->
        let p, children = advance p 1 |> _get_children in
        (match children with
        | None -> p, None
        | Some x ->
            let kind =
              match gptl_opt with
              | None -> Ast.Struct { name = name
                                   ; children = x
                                   ; attribute_list = attribs_opt }
              | Some y -> Ast.GenericStruct { name = name 
                                            ; generic_placeholder_type_list = y
                                            ; children = x
                                            ; attribute_list = attribs_opt } in
            let struct_ast : Ast.t = {
              kind = kind;
              pos = pos;
            } in
            p, Some struct_ast)
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
        | Token.Identifier "union" -> !__parse_union p
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

  _struct p
