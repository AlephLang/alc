open Parser_core
open Parser_expr

let parse_attribute p =
  let rec _arguments p first =
    match (peek p 0).kind with
    | Token.RParen -> advance p 1, Some []
    | Token.Eof -> add_error_eof p, None
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
          let p, expr = parse_expr p false in
          match expr with
          | None -> p, None
          | Some x ->
              let p, rest = _arguments p false in
              match rest with
              | None -> p, None
              | Some y -> p, Some (x :: y) in

  let pos = p.pos in

  (* attrib *) (* attrib (args...) *)
  (* ^~~~~~ *) (* ^~~~~~           *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier value ->
        let p = advance p 1 in
        (match (peek p 0).kind with
        | Token.LParen -> _lparen p value
        | _ ->
            let attrib_ast : Ast.t = {
              kind = Ast.Attribute { name = value; arguments = [] };
              pos = pos;
            } in
            p, Some attrib_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* attrib (args...) *)
  (*        ^~~~~~~~~ *)
  and _lparen p name =
    match (peek p 0).kind with
    | Token.LParen ->
        let p, arguments = _arguments (advance p 1) true in
        (match arguments with
        | None -> p, None
        | Some x ->
            let attrib_ast : Ast.t = {
              kind = Ast.Attribute { name = name; arguments = x };
              pos = pos;
            } in
            p, Some attrib_ast)
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None in

  _id p

let parse_attribute_list p =
  let rec _attributes p first =
    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.RBrack, Token.RBrack when not tok1.has_whitespace_after -> advance p 2, Some []
    | Token.RBrack, Token.RBrack ->
        advance (add_error_whitespace (advance p 1) Token.RBrack) 1, None
    | Token.RBrack, _ -> advance (add_error_unexpected (advance p 1) Token.RBrack) 1, None
    | Token.Eof, _ -> add_error_eof p, None
    | _ ->
        let p, success =
          if not first then
            match (peek p 0).kind with
            | Token.Comma -> advance p 1, true
            | Token.Eof -> add_error_eof p, false
            | _ ->
                (* NOTE: Adding LParen as a possible input because it is possible when parsing
                 * an attribute
                 *)
                advance (add_error_unexpected_s p [Token.Comma; Token.LParen]) 1, false
          else
            p, true in
        if not success then p, None
        else
          let p, attrib = parse_attribute p in
          match attrib with
          | None -> p, None
          | Some x ->
              let p, rest = _attributes p false in
              match rest with
              | None -> p, None
              | Some y -> p, Some (x :: y) in

  let tok1, tok2 = peek p 0, peek p 1 in
  match tok1.kind, tok2.kind with
  | Token.LBrack, Token.LBrack when not tok1.has_whitespace_after ->
      let pos = p.pos in
      let p, attribs = _attributes (advance p 2) true in
      (match attribs with
      | None -> p, None
      | Some x ->
          let attrib_list_ast : Ast.t = {
            kind = Ast.AttributeList { attributes = x };
            pos = pos;
          } in
          p, Some attrib_list_ast)
  | Token.LBrack, Token.LBrack -> advance (add_error_whitespace (advance p 1) Token.LBrack) 1, None
  | Token.LBrack, _ -> advance (add_error_unexpected (advance p 1) Token.LBrack) 1, None
  | Token.Eof, _ -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.LBrack) 1, None
