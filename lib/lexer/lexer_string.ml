open Lexer_core

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
