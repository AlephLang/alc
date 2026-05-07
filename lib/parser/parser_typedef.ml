open Parser_core
open Parser_type
open Parser_generic_type_list
open Util

let parse_typedef p =
  let pos = p.pos in
  let p = advance p 1 in
  match (peek p 0).kind with
  | Token.Identifier value ->
      let p = advance p 1 in
      let p, generic_type_list, success =
        match (peek p 0).kind with
        | Token.ExclMark ->
            let newp, type_list = parse_generic_type_list p in
            (match type_list with
            | Some _ -> newp, type_list, true
            | None -> newp, None, false)
        | _ -> p, None, true in
      if not success then p, None
      else
        (match (peek p 0).kind with
        | Token.Eq ->
            let p, type_ = advance p 1 |> parse_type in
            (match type_ with
            | Some x ->
                (match (peek p 0).kind with
                | Token.Semicolon -> 
                  let typedef_ast : Ast.t = {
                    kind = Ast.TypeDef {
                      name = value;
                      generic_type_list = generic_type_list;
                      aliased_type = x;
                      attribute_list = None;
                    };
                    pos = pos;
                  } in
                advance p 1, Some typedef_ast
                | Token.Eof -> add_error_eof p, None
                | _ -> advance (add_error_unexpected p @@ Token.Semicolon) 1, None)
            | None -> p, None)
        | Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p @@ Token.Eq) 1, None)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None
