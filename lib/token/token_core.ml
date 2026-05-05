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
