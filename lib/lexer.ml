open Char.Ascii

type t = {
  src: string;
  pos: int;
  line: int;
  llp: int;
  ch: char option;
}

let create src = {
  src = src;
  pos = 0;
  line = 0;
  llp = 0;
  ch = if String.length src = 0 then None else Some src.[0];
}

let is_start_of_id c = is_letter c || c = '_' || c = '$'

let is_part_of_id c = is_start_of_id c || is_digit c

let is_processable c =
  match c with
  | '.' | '!' | '*' | '/' | '+' | '-' | '^' | '&' | '|' | '(' | ')' | '[' | ']' | '{' | '}' | '"'
  | '\'' | '<' | '>' | '=' | '%' | '@' | ':' | ';' | '#' | ',' | '~' -> true
  | _ -> is_start_of_id c || is_digit c || Util.is_space c

let advance l =
  let pos = l.pos + 1 in
  let line, llp = match l.ch with
  | None -> l.line, l.llp
  | Some x -> if x = '\n' then l.line + 1, pos else l.line, l.llp in
  let ch = if String.length l.src > pos then Some l.src.[pos] else None in
  {
    src = l.src;
    pos = pos;
    line = line;
    llp = llp;
    ch = ch;
  }

let peek l x = if l.pos + x < String.length l.src then Some l.src.[l.pos + x] else None

let generate_token_and_advance l kind =
  let token : Token.t = {
    kind = kind;
    line = l.line;
    pos = l.pos - l.llp;
    length = 1;
    has_whitespace_after = Util.is_space @@ Option.value (peek l 1) ~default:' ';
  } in
  advance l, Some token

let rec seek l condition =
  if condition l.ch then
    let ch = String.make 1 @@ Option.value l.ch ~default:'?' in
    let l, remain = seek (advance l) condition in
    l, ch ^ remain
  else
    l, ""

let skip_whitespace l =
  let l, _ = seek l (fun x -> match x with
                     | None -> false
                     | Some c -> Util.is_space c) in
  l

let rec skip_c_comments l =
  match peek l 0, peek l 1 with
  | Some c1, Some c2 ->
      if c1 = '*' && c2 = '/' then advance l |> advance else advance l |> skip_c_comments
  | Some _, _ -> advance l |> skip_c_comments
  | _ -> l

let rec skip_cpp_comments l =
  match peek l 0 with
  | None -> l
  | Some c -> if c = '\n' then advance l else advance l |> skip_cpp_comments

let process_error l =
  let pos = l.pos in
  let l, value = seek l (fun x -> match x with
                         | None -> false
                         | Some c -> not @@ is_processable c) in
  let token : Token.t = {
    kind = Token.Error value;
    line = l.line;
    pos = pos - l.llp;
    length = l.pos - pos;
    has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
  } in
  l, Some token

let process_id l =
  let pos = l.pos in
  let l, value = seek l (fun x -> match x with
                         | None -> false
                         | Some c -> is_part_of_id c) in
  let token : Token.t = {
    kind = Token.Identifier value;
    line = l.line;
    pos = pos - l.llp;
    length = l.pos - pos;
    has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
  } in
  l, Some token

let rec process_str_seq l chr =
  match l.ch with
  | None -> l, ""
  | Some c1 ->
      if c1 = '\\' then
        match peek l 1 with
        | None -> advance l, "\\"
        | Some c2 ->
            if c2 = chr then
              let l, str = process_str_seq (advance l |> advance) chr in
              l, "\\" ^ String.make 1 chr ^ str
            else
              let l, str = process_str_seq (advance l) chr in
              l, "\\" ^ str
      else if c1 = chr then
        advance l, ""
      else
        let l, str = process_str_seq (advance l) chr in
        l, String.make 1 c1 ^ str

let process_string l =
  let pos, line, llp = l.pos, l.line, l.llp in
  let l, value = process_str_seq (advance l) '"' in
  let token : Token.t = {
    kind = Token.String value;
    line = line;
    pos = pos - llp;
    length = l.pos - pos;
    has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
  } in
  l, Some token

let process_symbol l =
  let pos, line, llp = l.pos, l.line, l.llp in
  let l, value = process_str_seq (advance l) '\'' in
  let token : Token.t = {
    kind = Token.Symbol value;
    line = line;
    pos = pos - llp;
    length = l.pos - pos;
    has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
  } in
  l, Some token

let process_num l =
  let _hex l =
    let pos = l.pos in
    let l = advance l |> advance in
    let l, value = seek l (fun x -> match x with
                           | None -> false
                           | Some c -> is_hex_digit c || c = '_' || c = '\'') in
    let x = ref 0 in
    for i = 0 to String.length value - 1 do
      let c = value.[i] in
      match c with
      | '\'' | '_' -> ()
      | _ ->
          let ascii_val = int_of_char c in
          if c >= 'a' && c <= 'f' then
            x := (!x lsl 4) lor (ascii_val - int_of_char 'a' + 0xA)
          else if c >= 'A' && c <= 'F' then
            x := (!x lsl 4) lor (ascii_val - int_of_char 'A' + 0xA)
          else
            x := (!x lsl 4) lor (ascii_val - int_of_char '0')
    done;
    let token : Token.t = {
      kind = Token.NumberHex !x;
      line = l.line;
      pos = pos - l.llp;
      length = l.pos - pos;
      has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
    } in
    l, Some token in

  let _bin l =
    let pos = l.pos in
    let l = advance l |> advance in
    let l, value = seek l (fun x -> match x with
                           | None -> false
                           | Some c -> c = '0' || c = '1' || c = '_' || c = '\'') in
    let x = ref 0 in
    for i = 0 to String.length value - 1 do
      let c = value.[i] in
      match c with
      | '\'' | '_' -> ()
      | _ -> x := (!x lsl 1) lor (int_of_char c - int_of_char '0')
    done;
    let token : Token.t = {
      kind = Token.NumberBin !x;
      line = l.line;
      pos = pos - l.llp;
      length = l.pos - pos;
      has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
    } in
    l, Some token in

  let _oct l =
    let pos = l.pos in
    let l = advance l in
    let l, value = seek l (fun x -> match x with
                           | None -> false
                           | Some c -> (c >= '0' && c <= '7') || c = '_' || c = '\'') in
    let x = ref 0 in
    for i = 0 to String.length value - 1 do
      let c = value.[i] in
      match c with
      | '\'' | '_' -> ()
      | _ -> x := (!x lsl 3) lor (int_of_char c - int_of_char '0')
    done;
    let token : Token.t = {
      kind = Token.NumberOct !x;
      line = l.line;
      pos = pos - l.llp;
      length = l.pos - pos;
      has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
    } in
    l, Some token in

  let _float_dec l =
    let pos = l.pos in
    let l, value, has_period =
      let rec _get_str l has_period =
        match l.ch with
        | None -> l, "", has_period
        | Some c ->
            match c with
            | '\'' | '_' -> _get_str (advance l) has_period
            | '.' ->
                if has_period then l, "", true
                else
                  let l, value, _ = _get_str (advance l) true in
                  l, "." ^ value, true
            | _ ->
                if is_digit c then
                  let l, value, has_period = _get_str (advance l) has_period in
                  l, String.make 1 c ^ value, has_period
                else
                  l, "", has_period in
      _get_str l false in
    let token : Token.t = {
      kind = if has_period then Token.NumberFloat (float_of_string value)
                           else Token.Number (int_of_string value);
      line = l.line;
      pos = pos - l.llp;
      length = l.pos - pos;
      has_whitespace_after = Util.is_space @@ Option.value l.ch ~default:' ';
    } in
    l, Some token in

  if Option.get l.ch = '0' then
    match peek l 1 with
    | None ->
        let token : Token.t = {
          kind = Token.Number 0;
          line = l.line;
          pos = l.pos - l.llp;
          length = 1;
          has_whitespace_after = true;
        } in
        advance l, Some token
    | Some c ->
        match c with
        | 'x' -> _hex l
        | 'b' -> _bin l
        | '.' | '\'' | '_' -> _float_dec l
        | _ ->
            if is_digit c then _oct l
            else
              let token : Token.t = {
                kind = Token.Number 0;
                line = l.line;
                pos = l.pos - l.llp;
                length = 1;
                has_whitespace_after = true;
              } in
              advance l, Some token
  else
    _float_dec l

let next_token l =
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
      if not @@ is_processable c then
        process_error l
      else if is_start_of_id c then
        process_id l
      else if is_digit c then
        process_num l
      else match c with
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
      | _ -> l, None

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
