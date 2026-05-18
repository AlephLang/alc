open Parser_core
open Parser_expr

let rec parse_initlist p =
  (* expr *)
  let _expression p =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x ->
        let initlist_entry : Ast.t = {
          kind = Ast.InitListEntry { expression = x };
          pos = x.pos;
        } in
        p, Some initlist_entry in

  (* { entries } *)
  let _nested_initlist p =
    let p, nested_initlist = parse_initlist p in
    match nested_initlist with
    | None -> p, None
    | Some x ->
        let initlist_entry : Ast.t = {
          kind = Ast.InitListEntry { expression = x };
          pos = x.pos;
        } in
        p, Some initlist_entry in

  (* .field_name = ... *)
  let _explicit_field p =
    let pos = p.pos in

    (* .field_name = ... *)
    (* ^~~~~~~~~~~       *)
    let rec __field_name p =
      let tok1, tok2 = peek p 0, peek p 1 in
      match tok1.kind, tok2.kind with
      | Token.Period, Token.Identifier value when not tok1.has_whitespace_after ->
          __eq (advance p 2) value
      | Token.Period, Token.Identifier _ ->
          advance (add_error_whitespace p @@ Token.Identifier "") 1, None
      | Token.Period, Token.Eof -> add_error_eof (advance p 1), None
      | Token.Period, _ ->
          advance (add_error_unexpected (advance p 1) @@ Token.Identifier "") 1, None
      | Token.Eof, _ -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.Period) 1, None

    (* .field_name = ... *)
    (*             ^     *)
    and __eq p field_name =
      match (peek p 0).kind with
      | Token.Eq -> __expr_or_initlist (advance p 1) field_name
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.Eq) 1, None

    (* .field_name = expr *) (* .field_name = { entries } *)
    (*               ^~~~ *) (*               ^~~~~~~~~~~ *)
    and __expr_or_initlist p field_name =
      let p, value =
        match (peek p 0).kind with
        | Token.LCBrack -> parse_initlist p
        | _ -> parse_expr p false in
      match value with
      | None -> p, None
      | Some x ->
          let initlist_entry_explicit : Ast.t = {
            kind = Ast.InitListEntryExplicit { name = field_name; expression = x };
            pos = pos;
          } in
          p, Some initlist_entry_explicit in

    __field_name p in

  (* [expr] = ... *)
  let _explicit_array_element p =
    let pos = p.pos in

    (* [expr] = ... *)
    (* ^            *)
    let rec __lbrack p =
      match (peek p 0).kind with
      | Token.LBrack -> advance p 1 |> __expr
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.LBrack) 1, None

    (* [expr] = ... *)
    (*  ^~~~        *)
    and __expr p =
      let p, expr = parse_expr p false in
      match expr with
      | None -> p, None
      | Some x -> __rbrack p x

    (* [expr] = ... *)
    (*      ^       *)
    and __rbrack p index_expr =
      match (peek p 0).kind with
      | Token.RBrack -> __eq (advance p 1) index_expr
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.RBrack) 1, None

    (* [expr] = ... *)
    (*        ^     *)
    and __eq p index_expr =
      match (peek p 0).kind with
      | Token.Eq -> __expr_or_initlist (advance p 1) index_expr
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.Eq) 1, None

    (* [expr] = expr *) (* [expr] = { entries } *)
    (*          ^~~~ *) (*          ^~~~~~~~~~~ *)
    and __expr_or_initlist p index_expr =
      let p, value =
        match (peek p 0).kind with
        | Token.LCBrack -> parse_initlist p
        | _ -> parse_expr p false in
      match value with
      | None -> p, None
      | Some x ->
          let initlist_entry_explicit_array_element : Ast.t = {
            kind = Ast.InitListEntryExplicitArrayElement { index_expression = index_expr
                                                         ; expression = x };
            pos = pos;
          } in
          p, Some initlist_entry_explicit_array_element in

    __lbrack p in

  let _entry p =
    match (peek p 0).kind with
    | Token.LCBrack -> _nested_initlist p
    | Token.Period -> _explicit_field p
    | Token.LBrack -> _explicit_array_element p
    | _ -> _expression p in

  let rec _entries p first =
    match (peek p 0).kind with
    | Token.RCBrack -> advance p 1, Some []
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
          let p, entry = _entry p in
          match entry with
          | None -> p, None
          | Some x ->
              let p, rest = _entries p false in
              match rest with
              | None -> p, None
              | Some y ->
                  p, Some (x :: y) in

  match (peek p 0).kind with
  | Token.LCBrack ->
      let pos = p.pos in
      let p, entries = _entries (advance p 1) true in
      (match entries with
      | None -> p, None
      | Some x ->
          let initlist_ast : Ast.t = {
            kind = Ast.InitList { entries = x };
            pos = pos;
          } in
          p, Some initlist_ast)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.LCBrack) 1, None
