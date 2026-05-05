type error = {
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

let add_error_eof p = add_error p { kind = UnexpectedEof; pos = p.pos; len = 1; }
let add_error_unexpected p expected_token_kind =
  add_error p { kind = Unexpected { expected = expected_token_kind }; pos = p.pos; len = 1; }

let peek (p: t) adv =
  if p.pos + adv < List.length p.tokens then Some (List.nth p.tokens @@ p.pos + adv) else None
