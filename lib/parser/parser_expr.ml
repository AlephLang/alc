open Parser_core
open Parser_misc

let __parse_generic_type_list : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_type : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_type_raw : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_initlist : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)

let parse_operator p =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.LArrow, Token.LArrow, Token.Eq
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after ->
        advance p 3, Some Ast.ExprOperator.AssignShlEq
  | Token.RArrow, Token.RArrow, Token.Eq
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after ->
        advance p 3, Some Ast.ExprOperator.AssignShrEq
  | Token.LArrow, Token.LArrow, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.BinaryShl
  | Token.RArrow, Token.RArrow, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.BinaryShr
  | Token.Plus, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignAddEq
  | Token.Minus, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignSubEq
  | Token.Asterisk, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignMulEq
  | Token.Slash, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignDivEq
  | Token.Percent, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignModEq
  | Token.Ampersand, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignAndEq
  | Token.Pipe, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignOrEq
  | Token.Circumflex, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.AssignXorEq
  | Token.Eq, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.CompareEq
  | Token.ExclMark, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.CompareNotEq
  | Token.LArrow, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.CompareLThanEq
  | Token.RArrow, Token.Eq, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.CompareGThanEq
  | Token.Ampersand, Token.Ampersand, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.BooleanAnd
  | Token.Pipe, Token.Pipe, _
    when not tok1.has_whitespace_after ->
        advance p 2, Some Ast.ExprOperator.BooleanOr
  | Token.Plus, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryAdd
  | Token.Minus, _, _ -> advance p 1, Some Ast.ExprOperator.BinarySub
  | Token.Asterisk, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryMul
  | Token.Slash, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryDiv
  | Token.Percent, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryMod
  | Token.Ampersand, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryAnd
  | Token.Pipe, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryOr
  | Token.Circumflex, _, _ -> advance p 1, Some Ast.ExprOperator.BinaryXor
  | _ -> p, None

let get_precedence operator =
  match Ast.ExprOperator.get_group operator with
  | Ast.ExprOperator.Assign -> 10, 15
  | Ast.ExprOperator.Boolean -> 60, 65
  | Ast.ExprOperator.Compare -> 70, 75
  | _ ->
      match operator with
      | Ast.ExprOperator.BinaryAdd
      | Ast.ExprOperator.BinarySub -> 20, 25
      | Ast.ExprOperator.BinaryMul
      | Ast.ExprOperator.BinaryDiv
      | Ast.ExprOperator.BinaryMod -> 30, 35
      | Ast.ExprOperator.BinaryShl
      | Ast.ExprOperator.BinaryShr -> 40, 45
      | Ast.ExprOperator.BinaryAnd
      | Ast.ExprOperator.BinaryOr
      | Ast.ExprOperator.BinaryXor -> 50, 55
      | _ -> Util.not_reached __FILE__ __LINE__

let is_namespace p =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.Identifier _, Token.Colon, Token.Colon
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after
      && not tok3.has_whitespace_after -> true
  | _ -> false

let is_generic_call_or_namespace p =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.Identifier _, Token.ExclMark, Token.LArrow
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after -> true
  | _ -> false
let is_generic_call p = is_generic_call_or_namespace p (* Alias just for convenience *)

let is_call p =
  match (peek p 0).kind, (peek p 1).kind with
  | Token.Identifier _, Token.LParen -> true
  | _ -> false

let rec parse_call_arguments p =
  let rec _args p first =
    match (peek p 0).kind with
    | Token.RParen -> advance p 1, Some []
    | Token.Eof -> add_error_eof p, None
    | _ ->
        let p, success =
          if not first then
            match (peek p 0).kind with
            | Token.Comma -> advance p 1, true
            | _ -> advance (add_error_unexpected p @@ Token.Comma) 1, false
          else p, true in
        if not success then p, None
        else
          let p, expr =
            match (peek p 0).kind with
            | Token.LCBrack -> !__parse_initlist p
            | _ -> parse_expr p false in
          match expr with
          | None -> p, None
          | Some x ->
              let p, remaining = _args p false in
              match remaining with
              | None -> p, None
              | Some y -> p, Some (x :: y) in

  match (peek p 0).kind with
  | Token.LParen -> _args (advance p 1) true
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.LParen) 1, None

and parse_operands p =
  let _typespec p =
    let tok0, tok1 = peek p @@ -1, peek p 0 in
    match tok1.kind with
    | Token.Identifier value when not tok0.has_whitespace_after -> advance p 1, Some value
    | _ -> p, None in

  match (peek p 0).kind with
  | Token.Identifier value ->
      (match value with
      | "sizeof" -> parse_sizeof p
      | "alignof" -> parse_alignof p
      | "cast" -> parse_cast p
      | _ -> parse_namespaces_and_identifier_operands p)
  | Token.Number value
  | Token.NumberHex value
  | Token.NumberBin value
  | Token.NumberOct value ->
      let pos = p.pos in
      let p, typespec = advance p 1 |> _typespec in
      let number_ast : Ast.t = {
        kind = Ast.ExprOperandNumber { value = value; typespec = typespec };
        pos = pos;
      } in
      p, Some number_ast
  | Token.NumberFloat value ->
      let pos = p.pos in
      let p, typespec = advance p 1 |> _typespec in
      let number_float_ast : Ast.t = {
        kind = Ast.ExprOperandNumberFloat { value = value; typespec = typespec };
        pos = pos;
      } in
      p, Some number_float_ast
  | Token.String contents ->
      let pos = p.pos in
      let p, typespec = advance p 1 |> _typespec in
      let string_ast : Ast.t = {
        kind = Ast.ExprOperandString { contents = contents; typespec = typespec };
        pos = pos;
      } in
      p, Some string_ast
  | Token.Symbol contents ->
      let pos = p.pos in
      let p, typespec = advance p 1 |> _typespec in
      let symbol_ast : Ast.t = {
        kind = Ast.ExprOperandSymbol { contents = contents; typespec = typespec };
        pos = pos;
      } in
      p, Some symbol_ast
  | Token.LParen ->
      let p, expr = parse_expr (advance p 1) false in
      (match expr, (peek p 0).kind with
      | Some _, Token.RParen -> advance p 1, expr
      | Some _, Token.Eof -> add_error_eof p, None
      | Some _, _ -> advance (add_error_unexpected p Token.RParen) 1, None
      | None, _ -> p, None)
  | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None

and parse_namespaces_and_identifier_operands p =
  if is_generic_call_or_namespace p then parse_generic_call_or_namespace p
  else if is_namespace p then parse_namespace p
  else if is_call p then parse_call p
  else parse_identifier p

and parse_only_operands p =
  if is_generic_call p then parse_generic_call p
  else if is_call p then parse_call p
  else parse_identifier p

and parse_post_identifier p ast =
  let parse_array p =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, expr = parse_expr (advance p 1) false in
        (match expr, (peek p 0).kind with
        | Some x, Token.RBrack -> advance p 1, expr
        | None, _ -> p, None
        | _, Token.Eof -> add_error_eof p, None
        | _ -> advance (add_error_unexpected p @@ Token.RBrack) 1, None)
    | _ -> p, None in

  let rec parse_arrays p ast =
    match (peek p 0).kind with
    | Token.LBrack ->
        let pos = p.pos in
        let p, expr = parse_array p in
        (match expr with
        | None -> p, None
        | Some x ->
            let ast : Ast.t = {
              kind = Ast.ExprOperandArrayElement { array = ast; index_expression = x };
              pos = pos;
            } in
            parse_arrays p ast)
    | _ -> p, Some ast in

  let parse_access p ast =
    match (peek p 0).kind with
    | Token.Period ->
        let pos = p.pos in
        let p, member = advance p 1 |> parse_only_operands in
        (match member with
        | None -> p, None
        | Some x ->
            let access_ast : Ast.t = {
              kind = Ast.ExprOperandAccessMember { from = ast; what = x };
              pos = pos;
            } in
            p, Some access_ast)
    | _ -> p, Some ast in

  let p, array_ast_opt = parse_arrays p ast in
  match array_ast_opt with
  | None -> p, None
  | Some x -> parse_access p x

and parse_identifier p =
  match (peek p 0).kind with
  | Token.Identifier value ->
      let identifier_ast : Ast.t = {
        kind = Ast.ExprOperandIdentifier { name = value };
        pos = p.pos;
      } in
      parse_post_identifier (advance p 1) identifier_ast
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

and parse_call p =
  (* Suppress warning about non-exhaustive pattern-matching *)
  let [@warning "-8"] Token.Identifier value = (peek p 0).kind in
  let pos = p.pos in
  let p, args = advance p 1 |> parse_call_arguments in
  match args with
  | None -> p, None
  | Some x ->
      let call_ast : Ast.t = {
        kind = Ast.ExprOperandCall { callee_name = value; arguments = x };
        pos = pos;
      } in
      parse_post_identifier p call_ast

and parse_generic_call p =
  (* Suppress warning about non-exhaustive pattern-matching *)
  let [@warning "-8"] Token.Identifier value = (peek p 0).kind in
  let pos = p.pos in
  let p, generic_type_list = advance p 1 |> !__parse_generic_type_list in
  match generic_type_list with
  | None -> p, None
  | Some x ->
      let p, args = parse_call_arguments p in
      match args with
      | None -> p, None
      | Some y ->
          let generic_call_ast : Ast.t = {
            kind = Ast.ExprOperandGenericCall { callee_name = value
                                              ; generic_type_list = x
                                              ; arguments = y };
            pos = pos;
          } in
          parse_post_identifier p generic_call_ast

and parse_namespace p =
  let tok1, tok2, tok3 = peek p 0, peek p 1, peek p 2 in
  match tok1.kind, tok2.kind, tok3.kind with
  | Token.Identifier value, Token.Colon, Token.Colon
    when not tok1.has_whitespace_after
      && not tok2.has_whitespace_after
      && not tok3.has_whitespace_after ->
        let pos = p.pos in
        let p, subobject = advance p 3 |> parse_namespaces_and_identifier_operands in
        (match subobject with
        | None -> p, None
        | Some x ->
            let namespace_ast : Ast.t = {
              kind = Ast.Namespace { name = value; subobject = x };
              pos = pos;
            } in
            p, Some namespace_ast)
  | _ -> Util.not_reached __FILE__ __LINE__

and parse_generic_call_or_namespace p =
  let advanced_p, generic_type_list = advance p 1 |> !__parse_generic_type_list in
  let tok0, tok1, tok2 = peek advanced_p @@ -1, peek advanced_p 0, peek advanced_p 1 in
  match generic_type_list, tok1.kind, tok2.kind with
  | Some _, Token.Colon, Token.Colon ->
      if tok0.has_whitespace_after then add_error_whitespace p Token.Colon, None
      else if tok1.has_whitespace_after then add_error_whitespace p Token.Colon, None
      else if tok2.has_whitespace_after then add_error_whitespace p @@ Token.Identifier "", None
      else parse_generic_namespace p
  | Some _, Token.LParen, _ -> parse_generic_call p
  | _, Token.Eof, _ -> add_error_eof p, None
  | Some _, _, _ -> advance (add_error_unexpected advanced_p @@ Token.LParen) 1, None
  | _ -> p, None

and parse_generic_namespace p =
  (* Suppress warning about non-exhaustive pattern-matching *)
  let [@warning "-8"] Token.Identifier value = (peek p 0).kind in
  let pos = p.pos in
  let p, generic_type_list = advance p 1 |> !__parse_generic_type_list in
  match generic_type_list with
  | None -> p, None
  | Some x ->
      let p, subobject = advance p 2 |> parse_identifier in
      match subobject with
      | None -> p, None
      | Some y ->
          let generic_namespace_ast : Ast.t = {
            kind = Ast.GenericNamespace { name = value; generic_type_list = x; subobject = y };
            pos = pos;
          } in
          p, Some generic_namespace_ast

and parse_sizeof p =
  let pos = p.pos in

  (* sizeof ( type ) *)
  (* ^~~~~~          *)
  let rec _sizeof p =
    match (peek p 0).kind with
    | Token.Identifier "sizeof" -> advance p 1 |> _lparen
    | Token.Identifier _ -> advance (add_error_value p "sizeof") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "sizeof") 1, None

  (* sizeof ( type ) *)
  (*        ^        *)
  and _lparen p =
    match (peek p 0).kind with
    | Token.LParen -> advance p 1 |> _type
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* sizeof ( type ) *)
  (*          ^~~~   *)
  and _type p =
    let p, type_ = !__parse_type p in
    match type_ with
    | None -> p, None
    | Some x -> _rparen p x

  (* sizeof ( type ) *)
  (*               ^ *)
  and _rparen p type_ =
    match (peek p 0).kind with
    | Token.RParen ->
        let sizeof_ast : Ast.t = {
          kind = Ast.ExprOperandSizeOf { type_ = type_ };
          pos = pos;
        } in
        advance p 1, Some sizeof_ast
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None in

  _sizeof p

and parse_alignof p =
  let pos = p.pos in

  (* alignof ( expr ) *)
  (* ^~~~~~~          *)
  let rec _alignof p =
    match (peek p 0).kind with
    | Token.Identifier "alignof" -> advance p 1 |> _lparen
    | Token.Identifier _ -> advance (add_error_value p "alignof") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "alignof") 1, None

  (* alignof ( expr ) *)
  (*         ^        *)
  and _lparen p =
    match (peek p 0).kind with
    | Token.LParen -> advance p 1 |> _expr
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* alignof ( expr ) *)
  (*           ^~~~   *)
  and _expr p =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x -> _rparen p x

  (* alignof ( expr ) *)
  (*                ^ *)
  and _rparen p expr =
    match (peek p 0).kind with
    | Token.RParen ->
        let alignof_ast : Ast.t = {
          kind = Ast.ExprOperandAlignOf { expression = expr };
          pos = pos;
        } in
        advance p 1, Some alignof_ast
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None in

  _alignof p

and parse_cast p =
  let pos = p.pos in

  (* cast ( type ) expr *)
  (* ^~~~               *)
  let rec _cast p =
    match (peek p 0).kind with
    | Token.Identifier "cast" -> advance p 1 |> _lparen
    | Token.Identifier _ -> advance (add_error_value p "cast") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "cast") 1, None

  (* cast ( type ) expr *)
  (*      ^             *)
  and _lparen p =
    match (peek p 0).kind with
    | Token.LParen -> advance p 1 |> _type
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.LParen) 1, None

  (* cast ( type ) expr *)
  (*        ^~~~        *)
  and _type p =
    let p, type_ = !__parse_type p in
    match type_ with
    | None -> p, None
    | Some x -> _rparen p x

  (* cast ( type ) expr *)
  (*             ^      *)
  and _rparen p type_ =
    match (peek p 0).kind with
    | Token.RParen -> _expr (advance p 1) type_
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.RParen) 1, None

  (* cast ( type ) expr *)
  (*               ^~~~ *)
  and _expr p type_ =
    let p, expr = parse_expr p false in
    match expr with
    | None -> p, None
    | Some x ->
        let cast_ast : Ast.t = {
          kind = Ast.ExprOperandCastTo { type_ = type_; expression = x };
          pos = pos;
        } in
        p, Some cast_ast in

  _cast p

and parse_prefix p =
  let pos = p.pos in
  let operator =
    match (peek p 0).kind with
    | Token.ExclMark -> Ast.ExprOperator.PrefixBooleanNot
    | Token.Tilde -> Ast.ExprOperator.PrefixNot
    | Token.Minus -> Ast.ExprOperator.PrefixNegative
    | Token.Asterisk -> Ast.ExprOperator.PrefixDereference
    | Token.Ampersand -> Ast.ExprOperator.PrefixAddress
    | _ -> Util.not_reached __FILE__ __LINE__ in
  let p, operand = advance p 1 |> parse_operands in
  match operand with
  | None -> p, None
  | Some x ->
      let prefix_expr : Ast.t = {
        kind = Ast.PrefixExpr { operator = operator; operand = x };
        pos = pos;
      } in
      p, Some prefix_expr

and parse_operands_or_prefix p =
  if is_prefix_operator_token (peek p 0).kind then parse_prefix p else parse_operands p

and parse_expr p is_toplevel =
  let rec _pratt_parse p min_prec has_assign =
    let p, lhs = parse_operands_or_prefix p in
    match lhs with
    | None -> p, None
    | Some x ->
        let rec _pratt p lhs has_assign =
          let saved_p = p in
          let p, operator = parse_operator p in
          match operator with
          | None -> p, Some lhs
          | Some operator ->
              let operator_group = Ast.ExprOperator.get_group operator in
              let has_assign_2 = match operator_group with
                                 | Ast.ExprOperator.Assign -> true
                                 | _ -> false in
              if has_assign_2 && not is_toplevel then
                let assign_operator_in_non_toplevel_error : Parser_core.error = {
                  kind = AssignOperatorInNonToplevelExpression;
                  pos = saved_p.pos;
                  len = 1; (* FIXME: Use real operator length *)
                } in
                add_error p assign_operator_in_non_toplevel_error, None
              else if has_assign && has_assign_2 then
                let two_assign_operators_error : Parser_core.error = {
                  kind = TwoAssignOperators;
                  pos = saved_p.pos;
                  len = 1; (* FIXME: Use real operator length *)
                } in
                add_error p two_assign_operators_error, None
              else
                let l_prec, r_prec = get_precedence operator in
                if l_prec < min_prec then saved_p, Some lhs
                else
                  let p, rhs = _pratt_parse p r_prec (has_assign || has_assign_2) in
                  match rhs with
                  | None -> p, None
                  | Some z ->
                    let lhs : Ast.t = {
                      kind = Ast.Expr { lhs = lhs; rhs = z; operator = operator };
                      pos = lhs.pos;
                    } in
                    _pratt p lhs (has_assign || has_assign_2) in
        _pratt p x has_assign in
  _pratt_parse p 0 false

let parse_stmt_expr p =
  let pos = p.pos in
  let p, expr = parse_expr p true in
  match expr with
  | None -> p, None
  | Some x ->
      match (peek p 0).kind with
      | Token.Semicolon ->
          let ast_stmt_expr : Ast.t = {
            kind = Ast.StmtExpr { expression = x };
            pos = pos;
          } in
          advance p 1, Some ast_stmt_expr
      | Token.Eof -> add_error_eof p, None
      | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None
