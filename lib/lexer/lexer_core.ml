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

open Char.Ascii

let is_start_of_id c = is_letter c || c = '_' || c = '$'

let is_part_of_id c = is_start_of_id c || is_digit c

let is_processable c =
  match c with
  | '.' | '!' | '*' | '/' | '+' | '-' | '^' | '&' | '|' | '(' | ')' | '[' | ']' | '{' | '}' | '"'
  | '\'' | '<' | '>' | '=' | '%' | '@' | ':' | ';' | '#' | ',' | '~' -> true
  | _ -> is_start_of_id c || is_digit c || Util.is_space c
