open Parser_core
open Parser_import
open Parser_typedef
open Parser_export
open Parser_struct
open Parser_enum
open Parser_union
open Parser_extern
open Parser_decldef
open Parser_attributes

let parse_top p =
  let token = peek p 0 in
  match token.kind with
  | Token.Semicolon ->
      let none_ast : Ast.t = {
        kind = Ast.None;
        pos = p.pos;
      } in
      advance p 1, Some none_ast
  | Token.Identifier value ->
      (match value with
      | "import" -> parse_import p
      | "using" -> parse_typedef p
      | "export" -> parse_export p
      | "struct" -> parse_struct p
      | "enum" -> parse_enum p
      | "union" -> parse_union p
      | "extern" -> parse_extern p
      | _ -> parse_decldef p None)
  | Token.LBrack ->
      let p, attribs = parse_attribute_list p in
      (match attribs with
      | None -> p, None
      | Some _ -> parse_decldef p attribs)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None
