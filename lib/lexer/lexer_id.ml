open Lexer_core

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
