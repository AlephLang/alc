open Parser_core
open Parser_function

let parse_export p =
  match (peek p 0).kind with
  | Token.Identifier value when value = "export" ->
      parse_function (advance p 1) None Ast.FunctionTypeExported
  | Token.Identifier _ -> advance (add_error_value p "export") 1, None
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "export") 1, None
