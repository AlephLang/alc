open Alcotest

let check_token token expected () =
  let got = Alc.Token.show token in
  check string "same strings" expected got

let suite =
  let gen_tok (kind: Alc.Token.kind) : Alc.Token.t = {
    kind = kind;
    line = 0;
    pos = 0;
    length = 0;
    has_whitespace_after = true;
  } in

  let expected_string str =
    Printf.sprintf "{ kind: %s, line: 0, pos: 0, length: 0, has_whitespace_after: true }" str in

  [ "error", `Quick,
    check_token (gen_tok @@ Alc.Token.Error "error")
    @@ expected_string "Error { \"error\" }"
  ; "identifier", `Quick,
    check_token (gen_tok @@ Alc.Token.Identifier "identifier")
    @@ expected_string "Identifier { \"identifier\" }"
  ; "number", `Quick,
    check_token (gen_tok @@ Alc.Token.Number 123)
    @@ expected_string "Number { 123 0x7B 0b1111011 0173 }"
  ; "numberhex", `Quick,
    check_token (gen_tok @@ Alc.Token.NumberHex 123)
    @@ expected_string "NumberHex { 123 0x7B 0b1111011 0173 }"
  ; "numberbin", `Quick,
    check_token (gen_tok @@ Alc.Token.NumberBin 123)
    @@ expected_string "NumberBin { 123 0x7B 0b1111011 0173 }"
  ; "numberoct", `Quick,
    check_token (gen_tok @@ Alc.Token.NumberOct 123)
    @@ expected_string "NumberOct { 123 0x7B 0b1111011 0173 }"
  ; "numberfloat", `Quick,
    check_token (gen_tok @@ Alc.Token.NumberFloat 3.14)
    @@ expected_string "NumberFloat { 3.140000 }"
  ; "string", `Quick,
    check_token (gen_tok @@ Alc.Token.String "string")
    @@ expected_string "String { \"string\" }"
  ; "string-with-spec", `Quick,
    check_token (gen_tok @@ Alc.Token.String "string\n\t\r")
    @@ expected_string "String { \"string\\n\\t\\r\" }"
  ; "symbol", `Quick,
    check_token (gen_tok @@ Alc.Token.Symbol "x")
    @@ expected_string "Symbol { 'x' }"
  ; "symbol-with-newline", `Quick,
    check_token (gen_tok @@ Alc.Token.Symbol "\n")
    @@ expected_string "Symbol { '\\n' }"
  ; "symbol-with-tab", `Quick,
    check_token (gen_tok @@ Alc.Token.Symbol "\t")
    @@ expected_string "Symbol { '\\t' }"
  ; "symbol-with-carriage-return", `Quick,
    check_token (gen_tok @@ Alc.Token.Symbol "\r")
    @@ expected_string "Symbol { '\\r' }"
  ; "lparen", `Quick, check_token (gen_tok Alc.Token.LParen) @@ expected_string "LParen"
  ; "rparen", `Quick, check_token (gen_tok Alc.Token.RParen) @@ expected_string "RParen"
  ; "lbrack", `Quick, check_token (gen_tok Alc.Token.LBrack) @@ expected_string "LBrack"
  ; "rbrack", `Quick, check_token (gen_tok Alc.Token.RBrack) @@ expected_string "RBrack"
  ; "lcbrack", `Quick, check_token (gen_tok Alc.Token.LCBrack) @@ expected_string "LCBrack"
  ; "rcbrack", `Quick, check_token (gen_tok Alc.Token.RCBrack) @@ expected_string "RCBrack"
  ; "larrow", `Quick, check_token (gen_tok Alc.Token.LArrow) @@ expected_string "LArrow"
  ; "rarrow", `Quick, check_token (gen_tok Alc.Token.RArrow) @@ expected_string "RArrow"
  ; "colon", `Quick, check_token (gen_tok Alc.Token.Colon) @@ expected_string "Colon"
  ; "semicolon", `Quick, check_token (gen_tok Alc.Token.Semicolon) @@ expected_string "Semicolon"
  ; "comma", `Quick, check_token (gen_tok Alc.Token.Comma) @@ expected_string "Comma"
  ; "period", `Quick, check_token (gen_tok Alc.Token.Period) @@ expected_string "Period"
  ; "ampersand", `Quick, check_token (gen_tok Alc.Token.Ampersand) @@ expected_string "Ampersand"
  ; "pipe", `Quick, check_token (gen_tok Alc.Token.Pipe) @@ expected_string "Pipe"
  ; "circumflex", `Quick, check_token (gen_tok Alc.Token.Circumflex) @@ expected_string "Circumflex"
  ; "tilde", `Quick, check_token (gen_tok Alc.Token.Tilde) @@ expected_string "Tilde"
  ; "exclmark", `Quick, check_token (gen_tok Alc.Token.ExclMark) @@ expected_string "ExclMark"
  ; "plus", `Quick, check_token (gen_tok Alc.Token.Plus) @@ expected_string "Plus"
  ; "minus", `Quick, check_token (gen_tok Alc.Token.Minus) @@ expected_string "Minus"
  ; "asterisk", `Quick, check_token (gen_tok Alc.Token.Asterisk) @@ expected_string "Asterisk"
  ; "slash", `Quick, check_token (gen_tok Alc.Token.Slash) @@ expected_string "Slash"
  ; "percent", `Quick, check_token (gen_tok Alc.Token.Percent) @@ expected_string "Percent"
  ; "eq", `Quick, check_token (gen_tok Alc.Token.Eq) @@ expected_string "Eq"
  ; "at", `Quick, check_token (gen_tok Alc.Token.At) @@ expected_string "At"
  ; "hash", `Quick, check_token (gen_tok Alc.Token.Hash) @@ expected_string "Hash"
  ]
