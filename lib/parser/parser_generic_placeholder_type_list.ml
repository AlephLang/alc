open Parser_core
open Parser_type

let parse_generic_placeholder_type p =
  let pos = p.pos in

  (* name (= type) *)
  (* ^~~~          *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier value ->
        let p = advance p 1 in
        (match (peek p 0).kind with
        | Token.Eq -> _default_type p value
        | _ -> _result p value None)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* name = type *)
  (*      ^ ^~~~ *)
  and _default_type p name =
    match (peek p 0).kind with
    | Token.Eq ->
        let p, type_ = advance p 1 |> parse_type in
        (match type_ with
        | None -> p, None
        | Some _ -> _result p name type_)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Eq) 1, None

  and _result p name default_type =
    let generic_placeholder_type_ast : Ast.t = {
      kind = Ast.GenericPlaceholderType { name = name; default_type = default_type };
      pos = pos;
    } in
    p, Some generic_placeholder_type_ast in

  _id p

let parse_generic_placeholder_type_list p =
  let rec _generic_placeholder_types p first =
    match (peek p 0).kind with
    | Token.RArrow -> advance p 1, Some []
    | _ ->
        let p, success =
          if not first then
            match (peek p 0).kind with
            | Token.Comma -> advance p 1, true
            | Token.Eof -> add_error_eof p, false
            | _ -> advance (add_error_unexpected p Token.Comma) 1, false
          else p, true in
        if not success then p, None
        else
          let p, generic_placeholder_type = parse_generic_placeholder_type p in
          match generic_placeholder_type with
          | None -> p, None
          | Some x ->
              let p, rest = _generic_placeholder_types p false in
              match rest with
              | None -> p, None
              | Some y -> p, Some (x :: y) in

  match (peek p 0).kind with
  | Token.LArrow ->
      let pos = p.pos in
      let p, types = _generic_placeholder_types (advance p 1) true in
      (match types with
      | None -> p, None
      | Some x ->
          let generic_placeholder_type_list : Ast.t = {
            kind = Ast.GenericPlaceholderTypeList { generic_placeholder_types = x };
            pos = pos;
          } in
          p, Some generic_placeholder_type_list)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.LArrow) 1, None
