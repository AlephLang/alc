open Lexer_core

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
