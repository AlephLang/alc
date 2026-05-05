open Lexer_core

let skip_whitespace l =
  let l, _ = seek l (fun x -> match x with
                     | None -> false
                     | Some c -> Util.is_space c) in
  l

