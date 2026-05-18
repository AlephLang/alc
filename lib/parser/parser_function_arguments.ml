open Parser_core
open Parser_expr
open Parser_decldef

let parse_variadic_arguments p =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.Period, Token.Period, Token.Period
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after ->
        let variadic_ast : Ast.t = {
          kind = Ast.Variadic;
          pos = p.pos;
        } in
        advance p 3, Some variadic_ast
  | Token.Period, Token.Period, Token.Period
    when not tok1.has_whitespace_after ->
      advance (add_error_whitespace (advance p 1) Token.Period) 1, None
  | Token.Period, Token.Period, Token.Period ->
      advance (add_error_whitespace p Token.Period) 1, None
  | Token.Period, Token.Period, Token.Eof -> add_error_eof (advance p 2), None
  | Token.Period, Token.Period, _ ->
      advance (add_error_unexpected (advance p 2) Token.Period) 1, None
  | Token.Period, Token.Eof, _ -> add_error_eof (advance p 1), None
  | Token.Period, _, _ -> advance (add_error_unexpected (advance p 1) Token.Period) 1, None
  | Token.Eof, _, _ -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.Period) 1, None

let parse_function_arguments p =
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
          else
            p, true in
        if not success then p, None
        else
          let p, decldef_or_variadic =
            match (peek p 0).kind with
            | Token.Period -> parse_variadic_arguments p
            | _ -> parse_decldef_var p None in
          match decldef_or_variadic with
          | None -> p, None
          | Some x ->
              let p, rest = _arguments p false in
              match rest with
              | None -> p, None
              | Some y -> p, Some (x :: y) in

  match (peek p 0).kind with
  | Token.LParen ->
      let pos = p.pos in
      let p, arguments = _arguments (advance p 1) true in
      (match arguments with
      | None -> p, None
      | Some x ->
          let args_ast : Ast.t = {
            kind = Ast.ArgumentList { arguments = x };
            pos = pos;
          } in
          p, Some args_ast)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.LParen) 1, None
