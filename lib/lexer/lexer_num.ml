open Lexer_core
open Char.Ascii

let process_num_hex l =
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
  l, Some token

let process_num_bin l =
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
  l, Some token

let process_num_oct l =
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
  l, Some token

let process_num_float_dec l =
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
  l, Some token

let process_num l =
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
        | 'x' -> process_num_hex l
        | 'b' -> process_num_bin l
        | '.' | '\'' | '_' -> process_num_float_dec l
        | _ ->
            if is_digit c then process_num_oct l
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
    process_num_float_dec l
