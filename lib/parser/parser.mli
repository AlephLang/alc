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
  | NonPrefixOperatorAtTheBeginningOfAnExpression
  | PrefixOperatorAfterAnOperand
  | AssignOperatorInNonToplevelExpression
  | TwoNonPrefixOperators
  | TwoOperands
  | TwoAssignOperators
  | LastNodeIsNotAnOperandInExpression

val create : Token.t list -> t
val parse : t -> t * Ast.t
