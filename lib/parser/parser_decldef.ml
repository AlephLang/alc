open Parser_core
open Parser_misc

let __parse_type : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_vardecl : (t -> Ast.t option -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_vardef : (t -> Ast.t option -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_function : (t -> Ast.t option -> Ast.function_type -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_generic_function : (t -> Ast.t option -> Ast.function_type -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)

let rec parse_decldef p attribs =
  let _default_func_or_generic p function_type =
    match (peek p 3).kind with
    | Token.LArrow -> !__parse_generic_function p attribs function_type
    | _ -> !__parse_function p attribs function_type in

  let _qualifier p name =
    let pos = p.pos in
    let p, qualified = parse_decldef (advance p 1) attribs in
    match qualified with
    | None -> p, None
    | Some x ->
        let qualifier_ast : Ast.t = {
          kind = Ast.Qualifier { name = name; qualified = x };
          pos = pos;
        } in
        p, Some qualifier_ast in

  match (peek p 0).kind with
  | Token.Identifier value when value = "func" ->
      _default_func_or_generic (advance p 1) Ast.FunctionTypeExplicit
  | Token.Identifier value ->
      if is_qualifier value then _qualifier p value
      else
        (match (peek p 1).kind, (peek p 2).kind with
        | Token.Colon, Token.Colon -> _default_func_or_generic p Ast.FunctionTypeDefault
        | Token.Colon, _ ->
            let p, decldef = parse_decldef_var p attribs in
            (match decldef with
            | None -> p, None
            | Some _ ->
                match (peek p 0).kind with
                | Token.Semicolon -> advance p 1, decldef
                | Token.Eof -> add_error_eof p, None
                | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None)
        | Token.Eof, _ -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

and parse_decldef_var p attribs =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.Identifier _, Token.Colon, Token.Eq
    when not tok2.has_whitespace_after -> !__parse_vardef p attribs
  | Token.Identifier _, Token.Colon, _ -> vardecldef p attribs
  | Token.Identifier _, Token.Eof, _ -> add_error_eof (advance p 1), None
  | Token.Identifier _, _, _ -> advance (add_error_unexpected (advance p 1) Token.Colon) 1, None
  | Token.Eof, _, _ -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

and vardecldef p attribs =
  let saved_p = p in

  (* name : ... ; *)
  (* ^~~~         *)
  let rec _id p =
    match (peek p 0).kind with
    | Token.Identifier value -> _colon (advance p 1) value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* name : type ... ; *) (* name := expr ; *)
  (*      ^            *) (*      ^~        *)
  and _colon p value =
    let tok1, tok2 = peek p 0, peek p 1 in
    match tok1.kind, tok2.kind with
    | Token.Colon, Token.Eq -> !__parse_vardef saved_p attribs
    | Token.Colon, _ -> _type (advance p 1) value
    | Token.Eof, _ -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Colon) 1, None

  (* name : type ... ; *)
  (*        ^~~~       *)
  and _type p value =
    let p, type_ = !__parse_type p in
    match type_ with
    | None -> p, None
    | Some x -> _eq_or_end p value x

  (* name : type = ... ; *)
  (*               ^~~~~ *)
  and _eq_or_end p value type_ =
    match (peek p 0).kind with
    | Token.Eq -> !__parse_vardef saved_p attribs
    | _ ->
        let vardecl_ast : Ast.t = {
          kind = Ast.VarDecl { name = value; type_ = type_; attribute_list = attribs };
          pos = saved_p.pos;
        } in
        p, Some vardecl_ast in

  _id p
