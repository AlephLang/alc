type kind =
  | Eof
  | Error of string
  | Identifier of string
  | Number of int
  | NumberHex of int
  | NumberBin of int
  | NumberOct of int
  | NumberFloat of float
  | String of string
  | Symbol of string
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LCBrack
  | RCBrack
  | LArrow
  | RArrow
  | Colon
  | Semicolon
  | Comma
  | Period
  | Ampersand
  | Pipe
  | Circumflex
  | Tilde
  | ExclMark
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Percent
  | Eq
  | At
  | Hash
and t = {
  kind: kind;
  line: int;
  pos: int;
  length: int;
  has_whitespace_after: bool;
}

let show token =
  let open Printf in
  let showkind kind =
    match kind with
    | Eof -> "Eof"
    | Error x -> sprintf "Error { \"%s\" }" x
    | Identifier x -> sprintf "Identifier { \"%s\" }" x
    | Number x -> sprintf "Number { %i 0x%X 0b%a %#o }" x x Util.pp_binint x x
    | NumberHex x -> sprintf "NumberHex { %i 0x%X 0b%a %#o }" x x Util.pp_binint x x
    | NumberBin x -> sprintf "NumberBin { %i 0x%X 0b%a %#o }" x x Util.pp_binint x x
    | NumberOct x -> sprintf "NumberOct { %i 0x%X 0b%a %#o }" x x Util.pp_binint x x
    | NumberFloat x -> sprintf "NumberFloat { %f }" x
    | String x -> sprintf "String { \"%s\" }" (Util.preproc_str x)
    | Symbol x -> sprintf "Symbol { '%s' }" (Util.preproc_str x)
    | LParen -> "LParen"
    | RParen -> "RParen"
    | LBrack -> "LBrack"
    | RBrack -> "RBrack"
    | LCBrack -> "LCBrack"
    | RCBrack -> "RCBrack"
    | LArrow -> "LArrow"
    | RArrow -> "RArrow"
    | Colon -> "Colon"
    | Semicolon -> "Semicolon"
    | Comma -> "Comma"
    | Period -> "Period"
    | Ampersand -> "Ampersand"
    | Pipe -> "Pipe"
    | Circumflex -> "Circumflex"
    | Tilde -> "Tilde"
    | ExclMark -> "ExclMark"
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Asterisk -> "Asterisk"
    | Slash -> "Slash"
    | Percent -> "Percent"
    | Eq -> "Eq"
    | At -> "At"
    | Hash -> "Hash" in
  sprintf "{ kind: %s, line: %i, pos: %i, length: %i, has_whitespace_after: %b }"
    (showkind token.kind) token.line token.pos token.length token.has_whitespace_after

let pp ppf token =
  show token
