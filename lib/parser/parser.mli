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
  | Unexpected of { expected_list: Token.kind list }
  | UnexpectedEof
  | UnexpectedValue of { expected_list: string list }
  | UnexpectedWhitespace of { expected: Token.kind }
  | AssignOperatorInNonToplevelExpression
  | TwoAssignOperators

val create : Token.t list -> t
val parse : t -> t * Ast.t
