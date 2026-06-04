type error = {
  kind: errorkind;
  pos: int;
  len: int;
}
and errorkind =
  | Unknown
  | Unexpected of { expected_list: Token.kind list }
  | UnexpectedEof
  | UnexpectedValue of { expected_list: string list }
  | UnexpectedWhitespace of { expected: Token.kind }
  | AssignOperatorInNonToplevelExpression
  | TwoAssignOperators
and t = {
  tokens: Token.t list;
  errors: error list;
  pos: int;
}

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

let add_error_eof p = add_error p { kind = UnexpectedEof; pos = p.pos; len = 1 }
let add_error_unexpected p expected_token_kind =
  add_error p { kind = Unexpected { expected_list = [expected_token_kind] }; pos = p.pos; len = 1 }
let add_error_unexpected_s p list =
  add_error p { kind = Unexpected { expected_list = list }; pos = p.pos; len = 1 }
let add_error_whitespace p expected_token_kind =
  add_error p {
    kind = UnexpectedWhitespace { expected = expected_token_kind };
    pos = p.pos;
    len = 1;
  }
let add_error_value p expected_value =
  add_error p { kind = UnexpectedValue { expected_list = [expected_value] }; pos = p.pos; len = 1 }
let add_error_value_s p list =
  add_error p { kind = UnexpectedValue { expected_list = list }; pos = p.pos; len = 1 }

let peek (p: t) adv =
  if p.pos + adv < List.length p.tokens then
    List.nth p.tokens @@ p.pos + adv
  else
    let last_token = List.nth p.tokens @@ List.length p.tokens - 1 in
    let eof_token : Token.t = {
      kind = Token.Eof;
      line = last_token.line;
      pos = last_token.pos + last_token.length;
      length = 1;
      has_whitespace_after = true;
    } in
    eof_token
