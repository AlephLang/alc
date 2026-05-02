open Alcotest

let check_lexer () =
  let rec _strs_to_str strs =
    match strs with
    | [] -> ""
    | x :: xs -> x ^ "\n" ^ _strs_to_str xs in

  let input =
    let _lines = [
      "\\\\\\\\ \\\\\\";
      "_ $123 abcxyz";
      "123 0x7b 0b1111011 0173";
      "0.123 .123 123. 3.14";
      "\"abc\" \"\\\"abc\\\"xyz\" \"";
      "\"";
      "'x' '\\'' '";
      "'";
      "()[]{}<>+-*/%&|~^=!@:;.,#";
    ] in
    _strs_to_str _lines in

  let expected =
    let _lines = [
      "(0) Token { kind: Error { \"\\\\\\\\\" }, line: 0, pos: 0, length: 4, has_whitespace_after: true }";
      "(1) Token { kind: Error { \"\\\\\\\" }, line: 0, pos: 5, length: 3, has_whitespace_after: true }";
      "(2) Token { kind: Identifier { \"_\" }, line: 1, pos: 0, length: 1, has_whitespace_after: true }";
      "(3) Token { kind: Identifier { \"$123\" }, line: 1, pos: 2, length: 4, has_whitespace_after: true }";
      "(4) Token { kind: Identifier { \"abcxyz\" }, line: 1, pos: 7, length: 6, has_whitespace_after: true }";
      "(5) Token { kind: Number { 123 0x7B 0b1111011 0173 }, line: 2, pos: 0, length: 3, has_whitespace_after: true }";
      "(6) Token { kind: NumberHex { 123 0x7B 0b1111011 0173 }, line: 2, pos: 4, length: 4, has_whitespace_after: true }";
      "(7) Token { kind: NumberBin { 123 0x7B 0b1111011 0173 }, line: 2, pos: 9, length: 9, has_whitespace_after: true }";
      "(8) Token { kind: NumberOct { 123 0x7B 0b1111011 0173 }, line: 2, pos: 19, length: 4, has_whitespace_after: true }";
      "(9) Token { kind: NumberFloat { 0.123000 }, line: 3, pos: 0, length: 5, has_whitespace_after: true }";
      "(10) Token { kind: NumberFloat { 0.123000 }, line: 3, pos: 6, length: 4, has_whitespace_after: true }";
      "(11) Token { kind: NumberFloat { 123.000000 }, line: 3, pos: 11, length: 4, has_whitespace_after: true }";
      "(12) Token { kind: NumberFloat { 3.140000 }, line: 3, pos: 16, length: 4, has_whitespace_after: true }";
      "(13) Token { kind: String { \"abc\" }, line: 4, pos: 0, length: 5, has_whitespace_after: true }";
      "(14) Token { kind: String { \"\\\"abc\\\"xyz\" }, line: 4, pos: 6, length: 12, has_whitespace_after: true }";
      "(15) Token { kind: String { \"\\n\" }, line: 4, pos: 19, length: 3, has_whitespace_after: true }";
      "(16) Token { kind: Symbol { 'x' }, line: 6, pos: 0, length: 3, has_whitespace_after: true }";
      "(17) Token { kind: Symbol { '\\'' }, line: 6, pos: 4, length: 4, has_whitespace_after: true }";
      "(18) Token { kind: Symbol { '\\n' }, line: 6, pos: 9, length: 3, has_whitespace_after: true }";
      "(19) Token { kind: LParen, line: 8, pos: 0, length: 1, has_whitespace_after: false }";
      "(20) Token { kind: RParen, line: 8, pos: 1, length: 1, has_whitespace_after: false }";
      "(21) Token { kind: LBrack, line: 8, pos: 2, length: 1, has_whitespace_after: false }";
      "(22) Token { kind: RBrack, line: 8, pos: 3, length: 1, has_whitespace_after: false }";
      "(23) Token { kind: LCBrack, line: 8, pos: 4, length: 1, has_whitespace_after: false }";
      "(24) Token { kind: RCBrack, line: 8, pos: 5, length: 1, has_whitespace_after: false }";
      "(25) Token { kind: LArrow, line: 8, pos: 6, length: 1, has_whitespace_after: false }";
      "(26) Token { kind: RArrow, line: 8, pos: 7, length: 1, has_whitespace_after: false }";
      "(27) Token { kind: Plus, line: 8, pos: 8, length: 1, has_whitespace_after: false }";
      "(28) Token { kind: Minus, line: 8, pos: 9, length: 1, has_whitespace_after: false }";
      "(29) Token { kind: Asterisk, line: 8, pos: 10, length: 1, has_whitespace_after: false }";
      "(30) Token { kind: Slash, line: 8, pos: 11, length: 1, has_whitespace_after: false }";
      "(31) Token { kind: Percent, line: 8, pos: 12, length: 1, has_whitespace_after: false }";
      "(32) Token { kind: Ampersand, line: 8, pos: 13, length: 1, has_whitespace_after: false }";
      "(33) Token { kind: Pipe, line: 8, pos: 14, length: 1, has_whitespace_after: false }";
      "(34) Token { kind: Tilde, line: 8, pos: 15, length: 1, has_whitespace_after: false }";
      "(35) Token { kind: Circumflex, line: 8, pos: 16, length: 1, has_whitespace_after: false }";
      "(36) Token { kind: Eq, line: 8, pos: 17, length: 1, has_whitespace_after: false }";
      "(37) Token { kind: ExclMark, line: 8, pos: 18, length: 1, has_whitespace_after: false }";
      "(38) Token { kind: At, line: 8, pos: 19, length: 1, has_whitespace_after: false }";
      "(39) Token { kind: Colon, line: 8, pos: 20, length: 1, has_whitespace_after: false }";
      "(40) Token { kind: Semicolon, line: 8, pos: 21, length: 1, has_whitespace_after: false }";
      "(41) Token { kind: Period, line: 8, pos: 22, length: 1, has_whitespace_after: false }";
      "(42) Token { kind: Comma, line: 8, pos: 23, length: 1, has_whitespace_after: false }";
      "(43) Token { kind: Hash, line: 8, pos: 24, length: 1, has_whitespace_after: true }";
    ] in
    _strs_to_str _lines in

  let _, tokens = Alc.Lexer.create input |> Alc.Lexer.tokenize in
  let token_strs =
    let rec _get_strs tokens i =
      match tokens with
      | [] -> ""
      | x :: xs -> Printf.sprintf "(%i) Token %a" i Alc.Token.pp x ^ "\n" ^ _get_strs xs @@ i + 1 in
    _get_strs tokens 0 in
  check string "" expected token_strs

let suite = [ "tokenize", `Quick, check_lexer ]
