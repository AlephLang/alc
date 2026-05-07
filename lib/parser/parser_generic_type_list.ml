open Parser_core
open Parser_type

let rec parse_types_in_generic_type_list p first =
  match (peek p 0).kind with
  | Token.RArrow -> p, Some []
  | Token.Eof -> add_error_eof p, None
  | _ ->
      let p, can_continue =
        if not first then
          match (peek p 0).kind with
          | Token.Comma -> advance p 1, true
          | _ -> advance (add_error_unexpected p @@ Token.Comma) 1, false
        else p, true in
      if not can_continue then p, None
      else
        let p, type_ = parse_type p in
        match type_ with
        | None -> p, None
        | Some x ->
            let p, remaining = parse_types_in_generic_type_list p false in
            match remaining with
            | None -> p, None
            | Some y -> p, Some (x :: y)

let parse_generic_type_list p =
  let pos = p.pos in
  let tok1, tok2 = peek p 0, peek p 1 in
  match tok1.kind, tok2.kind with
  | Token.ExclMark, Token.LArrow when not tok1.has_whitespace_after ->
      let p, types = parse_types_in_generic_type_list (advance p 2) true in
      (match types with
      | Some x ->
          (match (peek p 0).kind with
          | Token.RArrow ->
              let generic_type_list_ast : Ast.t = {
                kind = Ast.GenericTypeList { types = x };
                pos = pos;
              } in
              advance p 1, Some generic_type_list_ast
          | Token.Eof -> add_error_eof p, None
          | _ -> advance (add_error_unexpected p @@ Token.RArrow) 1, None)
      | None -> p, None)
  | Token.Eof, _ -> add_error_eof p, None
  | _, _ -> advance (add_error_unexpected p @@ Token.ExclMark) 1, None
