let is_prefix_operator_token (kind : Token.kind) =
  match kind with
  | Token.ExclMark | Token.Tilde | Token.Minus | Token.Asterisk | Token.Ampersand -> true
  | _ -> false

let is_binary_operator_token (kind : Token.kind) =
  match kind with
  | Token.Plus | Token.Minus | Token.Asterisk | Token.Slash | Token.Percent | Token.Ampersand
  | Token.Pipe | Token.Circumflex -> true
  | _ -> false

let is_numeric_token (kind : Token.kind) =
  match kind with
  | Token.Number _
  | Token.NumberHex _
  | Token.NumberBin _
  | Token.NumberOct _
  | Token.NumberFloat _ -> true
  | _ -> false
