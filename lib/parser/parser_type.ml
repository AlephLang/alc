open Parser_core

let rec get_pointer_indirections p =
  match (peek p 0).kind with
  | Token.Asterisk ->
      let p, remaining = advance p 1 |> get_pointer_indirections in
      p, 1 + remaining
  | _ -> p, 0

let rec apply_pointer_type raw_pos pointer_indirections ast =
  if pointer_indirections = 0 then ast
  else
    let ast = apply_pointer_type raw_pos (pointer_indirections - 1) ast in
    let out_ast : Ast.t = {
      kind = Ast.TypePointer { type_ = ast };
      pos = raw_pos - pointer_indirections;
    } in
    out_ast

let parse_type_raw p =
  (* TODO:
   * - Function pointers
   * - Namespaces
   * - Generic namespaces
   *)
  let p, pointer_indirections = get_pointer_indirections p in
  let pos = p.pos in
  match (peek p 0).kind with
  | Token.Identifier value ->
      let ast_type_plain : Ast.t = {
        kind = Ast.TypePlain { name = value };
        pos = p.pos;
      } in
      let ast_type_plain = apply_pointer_type pos pointer_indirections ast_type_plain in
      advance p 1, Some ast_type_plain
  | Token.LParen -> Util.todo __FILE__ __LINE__ "Function pointer type."
  | _ -> p, None

let rec parse_type_array p ast =
  match (peek p 0).kind with
  | Token.LBrack ->
      let pos = p.pos in
      let p = advance p 1 in
      let p, size_expression, success = 
        match (peek p 0).kind with
        | Token.RBrack -> p, None, true
        | _ -> Util.todo __FILE__ __LINE__ "Array type with explicit size." in
      if not success then p, None, false
      else
        (match (peek p 0).kind with
        | Token.RBrack ->
            let arr : Ast.t = {
              kind = Ast.TypeArray {
                type_ = ast;
                size_expression = size_expression;
              };
              pos = pos;
            } in
            parse_type_array (advance p 1) arr
        | Token.Eof -> add_error_eof p, None, false
        | _ -> advance (add_error_unexpected p @@ Token.RBrack) 1, None, false)
  | _ -> p, Some ast, true

let parse_type p =
  let p, type_raw = parse_type_raw p in
  match type_raw with
  | None -> p, None
  | Some x ->
      let p, array_type, success = parse_type_array p x in
      if not success then p, None
      else
        match array_type with
        | Some y -> p, array_type
        | None -> p, None
