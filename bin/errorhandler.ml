type t = {
  src: string list;
  tokens: Alc.Token.t list;
  lnoffset: int;
}

let create src =
  let src_list = ref [] in
  let buf = ref "" in
  for i = 0 to String.length src - 1 do
    let c = src.[i] in
    if c = '\n' || i = String.length src - 1 then begin
      src_list := !src_list @ [!buf];
      buf := ""
    end else
      buf := !buf ^ String.make 1 c
  done;
  { src = !src_list; tokens = []; lnoffset = Alc.Util.number_length @@ List.length !src_list }

let set_tokens handler tokens = { src = handler.src; tokens = tokens; lnoffset = handler.lnoffset; }

let token_kind_to_string kind =
  let open Alc.Token in
  match kind with
  | Eof -> "Eof"
  | Error _ -> ""
  | Identifier value -> value
  | Number value
  | NumberHex value
  | NumberBin value
  | NumberOct value -> string_of_int value
  | NumberFloat value -> string_of_float value
  | String value -> value
  | Symbol value -> value
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LCBrack -> "{"
  | RCBrack -> "}"
  | LArrow -> "<"
  | RArrow -> ">"
  | Colon -> ":"
  | Semicolon -> ";"
  | Comma -> ","
  | Period -> "."
  | Ampersand -> "&"
  | Pipe -> "|"
  | Circumflex -> "^"
  | Tilde -> "~"
  | ExclMark -> "!"
  | Plus -> "+"
  | Minus -> "-"
  | Asterisk -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | Eq -> "="
  | At -> "@"
  | Hash -> "#"

let insert_at dst src index =
  if index = 0 then
    src ^ dst
  else if index = String.length dst then
    dst ^ src
  else
    let out = ref "" in
    for i = 0 to String.length dst - 1 do
      if i = index then begin
        out := !out ^ src
      end;
      out := !out ^ String.make 1 dst.[i];
    done;
    !out

let get_message_start filename line start msg ansi =
  Printf.sprintf "%s%s:%u:%u: %s%s: %s"
    (Ansi.to_ansi [Ansi.Graphics_Bold])
    filename
    (line + 1)
    start
    (Ansi.to_ansi ansi)
    msg
    (Ansi.to_ansi [])

let get_highlighted_token h (token: Alc.Token.t) ansi msg_after =
  let line = List.nth h.src token.line in
  let token_len = min (String.length line - token.pos) token.length in
  let line_num = token.line + 1 in
  let line_num_len = Alc.Util.number_length line_num in
  let line = insert_at line
                       (Ansi.to_ansi [])
                       (token.pos + token_len) in
  let line = insert_at line (Ansi.to_ansi ansi) token.pos in
  let line = " " ^ string_of_int line_num ^ " | " ^ line in
  let mark = String.make token.pos ' ' ^ Ansi.to_ansi ansi in
  let mark = mark ^ String.make 1 '^' in
  let mark = mark ^ String.make (token_len - 1) '~' in
  let mark = mark ^ " " ^ msg_after in
  let mark = mark ^ Ansi.to_ansi [] in
  let mark = " " ^ String.make line_num_len ' ' ^ " | " ^ mark in
  line ^ "\n" ^ mark

let rec handle_lexer_errors h filename (errors: Alc.Token.t list) =
  match errors with
  | [] -> ()
  | x :: xs ->
      match x.kind with
      | Alc.Token.Error value ->
        let msg_start = get_message_start filename
                                          x.line
                                          x.pos
                                          "error"
                                          [Ansi.Graphics_Bold;Ansi.Color_Red] in
        let msg = "unidentified token '"
                ^ Ansi.to_ansi [Ansi.Graphics_Bold]
                ^ value
                ^ Ansi.to_ansi []
                ^ "'\n" in
        let highlight = get_highlighted_token h x [Ansi.Color_Red; Ansi.Graphics_Bold] "" in
        print_endline @@ msg_start ^ msg ^ highlight;
        handle_lexer_errors h filename xs
      | _ -> handle_lexer_errors h filename xs

let get_parser_error_reason (h: t) (error: Alc.Parser.error) =
  let open Alc.Parser in
  match error.kind with
  | Unknown -> "<unknown error>"
  | Unexpected _ ->
      "unexpected token '"
      ^ Ansi.to_ansi [Ansi.Graphics_Bold]
      ^ token_kind_to_string (List.nth h.tokens error.pos).kind
      ^ Ansi.to_ansi []
      ^ "'"
  | UnexpectedEof -> "unexpected end of file"
  | InvalidExpression -> "invalid expression"
  | UnexpectedWhitespace _ -> "unexpected whitespace"
  | NoCharAfterBackslash -> "no symbol after backslash"
  | UnknownSpecialCharacter -> "unknown special character"
  | ExpressionIsEmpty -> "expression is empty"
  | NoOperandAfterPrefixOperatorInExpression -> "no operand after prefix operator in expression"
  | NonPrefixOperatorAtTheBeginningOfAnExpression ->
      "non-prefix operator at the beginning of expression"
  | PrefixOperatorAfterAnOperand -> "prefix operator after an operand"
  | AssignOperatorInNonToplevelExpression -> "assign operator in non-toplevel expression"
  | TwoNonPrefixOperators -> "two non-prefix operators in a row"
  | TwoOperands -> "two operands in a row"
  | TwoAssignOperators -> "two assign operators in one expression"
  | LastNodeIsNotAnOperandInExpression -> "last node in expression is not an operand"

let rec handle_parser_errors h filename (errors: Alc.Parser.error list) =
  match errors with
  | [] -> ()
  | x :: xs ->
      match x.kind with
      | Alc.Parser.UnexpectedEof ->
          let line = List.length h.src in
          let pos =
            match h.tokens with
            | [] -> 0
            | _ -> (List.nth h.tokens (List.length h.tokens - 1)).pos in
          let msg_start = get_message_start filename
                                            line
                                            pos
                                            "error"
                                            [Ansi.Graphics_Bold; Ansi.Color_Red] in
          print_endline @@ msg_start ^ "Unexpected end of file\n";
          handle_parser_errors h filename xs
      | _ ->
          let token = List.nth h.tokens x.pos in
          let msg_start = get_message_start filename
                                            token.line
                                            token.pos
                                            "error"
                                            [Ansi.Graphics_Bold; Ansi.Color_Red] in
          let msg = get_parser_error_reason h x ^ "\n" in
          let msg_after =
            match x.kind with
            | Alc.Parser.Unexpected { expected }
            | Alc.Parser.UnexpectedWhitespace { expected }->
                token_kind_to_string expected
            | _ -> "" in
          let highlight = get_highlighted_token h
                                                token
                                                [Ansi.Color_Red; Ansi.Graphics_Bold]
                                                msg_after in
          print_endline @@ msg_start ^ msg ^ highlight;
          handle_parser_errors h filename xs
