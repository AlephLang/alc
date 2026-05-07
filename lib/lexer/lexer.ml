include Lexer_core
open Lexer_comments
open Lexer_error
open Lexer_id
open Lexer_num
open Lexer_string
open Lexer_whitespace

let next_token l =
  let open Char.Ascii in
  let l = skip_whitespace l in
  match l.ch with
  | None ->
      let eof_token : Token.t = {
        kind = Token.Eof;
        line = l.line;
        pos = l.pos;
        length = 1;
        has_whitespace_after = true;
      } in
      l, Some eof_token
  | Some c ->
      match c with
      | '/' ->
          (match peek l 1 with
          | None -> generate_token_and_advance l Token.Slash
          | Some x ->
              match x with
              | '/' -> skip_cpp_comments l, None
              | '*' -> skip_c_comments l, None
              | _ -> generate_token_and_advance l Token.Slash)
      | '"' -> process_string l
      | '\'' -> process_symbol l
      | '(' -> generate_token_and_advance l Token.LParen
      | ')' -> generate_token_and_advance l Token.RParen
      | '[' -> generate_token_and_advance l Token.LBrack
      | ']' -> generate_token_and_advance l Token.RBrack
      | '{' -> generate_token_and_advance l Token.LCBrack
      | '}' -> generate_token_and_advance l Token.RCBrack
      | '<' -> generate_token_and_advance l Token.LArrow
      | '>' -> generate_token_and_advance l Token.RArrow
      | '+' -> generate_token_and_advance l Token.Plus
      | '-' -> generate_token_and_advance l Token.Minus
      | '*' -> generate_token_and_advance l Token.Asterisk
      | '%' -> generate_token_and_advance l Token.Percent
      | '&' -> generate_token_and_advance l Token.Ampersand
      | '|' -> generate_token_and_advance l Token.Pipe
      | '~' -> generate_token_and_advance l Token.Tilde
      | '^' -> generate_token_and_advance l Token.Circumflex
      | '=' -> generate_token_and_advance l Token.Eq
      | '!' -> generate_token_and_advance l Token.ExclMark
      | '@' -> generate_token_and_advance l Token.At
      | ':' -> generate_token_and_advance l Token.Colon
      | ';' -> generate_token_and_advance l Token.Semicolon
      | '.' ->
          (match peek l 1 with
          | None -> generate_token_and_advance l Token.Period
          | Some x ->
              if is_digit x then process_num l
              else generate_token_and_advance l Token.Period)
      | ',' -> generate_token_and_advance l Token.Comma
      | '#' -> generate_token_and_advance l Token.Hash
      | _ ->
          if not @@ is_processable c then process_error l
          else if is_start_of_id c then process_id l
          else if is_digit c then process_num l
          else Util.not_reached __FILE__ __LINE__

let rec tokenize l =
  let l, token = next_token l in
  match token with
  | None -> tokenize l
  | Some x ->
      match x.kind with
      | Token.Eof -> l, [], []
      | Token.Error _ ->
          let l, tokens, errors = tokenize l in
          l, tokens, x :: errors
      | _ ->
          let l, tokens, errors = tokenize l in
          l, x :: tokens, errors
