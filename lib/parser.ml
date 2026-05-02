type t = {
  tokens: Token.t list;
  errors: error list;
  pos: int;
}
and error = {
  kind: errorkind;
  pos: int;
  len: int;
}
and errorkind =
  | Unknown
  | Unexpected of { expected: Token.kind }
  | UnexpectedEof
  | InvalidExpression
  | UnexpectedWhitespace of { expected: Token.kind }
  | NoCharAfterBackslash
  | UnknownSpecialCharacter
  | ExpressionIsEmpty
  | NoOperandAfterPrefixOperatorInExpression
  | PrefixOperatorAfterAnOperand
  | AssignOperatorInNonToplevelExpression
  | TwoNonPrefixOperators
  | TwoOperands
  | TwoAssignOperators
  | LastNodeIsNotAnOperandInExpression

let create tokens = {
  tokens = tokens;
  errors = [];
  pos = 0;
}

let advance p adv = {
  tokens = p.tokens;
  errors = p.errors;
  pos = p.pos + adv;
}

let add_error p error = {
  tokens = p.tokens;
  errors = p.errors @ [error];
  pos = p.pos;
}

let add_error_eof p = add_error p { kind = UnexpectedEof; pos = p.pos; len = 1; }
let add_error_unexpected p expected_token_kind =
  add_error p { kind = Unexpected { expected = expected_token_kind }; pos = p.pos; len = 1; }

let peek p adv =
  if p.pos + adv < List.length p.tokens then Some (List.nth p.tokens @@ p.pos + adv) else None

let rec parse_module p =
  let pos = p.pos in
  match peek p 0 with
  | None -> add_error_eof p, None
  | Some x ->
      match x.kind with
      | Token.Identifier value ->
          let p = advance p 1 in
          let p, subm =
            match peek p 0, peek p 1 with
            | Some i, Some k ->
                (match i.kind, k.kind with
                | Token.Colon, Token.Colon
                  when not i.has_whitespace_after && not k.has_whitespace_after ->
                      advance p 2 |> parse_module
                | _, _ -> p, None)
            | _, _ -> p, None in
          let module_ast : Ast.t = {
            kind = Ast.Module { name = value; subm = subm };
            pos = pos;
          } in
          p, Some module_ast
      | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

let parse_import p =
  let pos = p.pos in
  let p, m = advance p 1 |> parse_module in
  match m with
  | None -> p, None
  | Some x ->
      (match peek p 0 with
      | None -> add_error_eof p, None
      | Some y ->
          match y.kind with
          | Token.Semicolon ->
              let import_ast : Ast.t = {
                kind = Ast.Import { m = x };
                pos = pos;
              } in
              advance p 1, Some import_ast
          | _ -> advance (add_error_unexpected p @@ Token.Semicolon) 1, None)

let parse_top p =
  match peek p 0 with
  | None -> add_error_eof p, None
  | Some x ->
      match x.kind with
      | Token.Semicolon ->
          let none_ast : Ast.t = {
            kind = Ast.None;
            pos = p.pos;
          } in
          advance p 1, Some none_ast
      | Token.Identifier value ->
          (match value with
          | "import" -> parse_import p
          | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None)
      | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None

let parse parser =
  let rec _toplevels p =
    if p.pos >= List.length p.tokens then p, []
    else
      let p, toplevel = parse_top p in
      let p, remaining = _toplevels p in
      match toplevel with
      | None -> p, remaining
      | Some x -> p, x :: remaining in

  let parser, toplevels = _toplevels parser in
  let root : Ast.t = {
    kind = Ast.Root { toplevel_statements = toplevels };
    pos = 0;
  } in
  parser, root
