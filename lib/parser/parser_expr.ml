open Parser_core
open Parser_misc

(* Fuck ocaml modules *)
let __parse_generic_type_list : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_type : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_type_raw : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_initlist : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)

type __ast_op_union =
  | Ast of Ast.t
  | Op of { pos: int; kind: Ast.ExprOperator.t }

let get_binding_power operator =
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

let token_to_expr_operator tokenkind expr_operator_group =
  match expr_operator_group with
  | Ast.ExprOperator.Binary ->
      (match tokenkind with
      | Token.Plus -> Ast.ExprOperator.BinaryAdd
      | Token.Minus -> Ast.ExprOperator.BinarySub
      | Token.Asterisk -> Ast.ExprOperator.BinaryMul
      | Token.Slash -> Ast.ExprOperator.BinaryDiv
      | Token.Percent -> Ast.ExprOperator.BinaryMod
      | Token.Ampersand -> Ast.ExprOperator.BinaryAnd
      | Token.Pipe -> Ast.ExprOperator.BinaryOr
      | Token.Circumflex -> Ast.ExprOperator.BinaryXor
      | _ -> Util.not_reached __FILE__ __LINE__)
  | Ast.ExprOperator.Prefix ->
      (match tokenkind with
      | Token.Asterisk -> Ast.ExprOperator.PrefixDereference
      | Token.ExclMark -> Ast.ExprOperator.PrefixBooleanNot
      | Token.Ampersand -> Ast.ExprOperator.PrefixAddress
      | Token.Minus -> Ast.ExprOperator.PrefixNegative
      | Token.Tilde -> Ast.ExprOperator.PrefixNot
      | _ -> Util.not_reached __FILE__ __LINE__)
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

and parse_expr_data p =
  (* WARNING: Things that come after are very messy *)
  let rec _gather_data p prev =
    let cur = peek p 0 in
    match cur.kind with
    | Token.Eof -> p, Some []
    | _ ->
        (* Prefix operator *)
        if ((match prev with
             | Ast x -> (match x.kind with
                         | Ast.None -> true
                         | _ -> false)
             | Op _ -> true) && is_prefix_operator_token cur.kind) then
          let out_op : __ast_op_union = Op {
            pos = p.pos;
            kind = token_to_expr_operator cur.kind Ast.ExprOperator.Prefix;
          } in
          _continue_gathering_op (advance p 1) out_op
        (* Operators *)
        else if match prev with
                | Op _ -> false
                | Ast x -> match x.kind with
                           | Ast.None -> false
                           | _ -> true then
          let tok1, tok2, tok3 = cur, peek p 1, peek p 2 in
          match tok1.kind, tok2.kind, tok3.kind with
          | Token.LArrow, Token.LArrow, Token.Eq
            when not tok1.has_whitespace_after
              && not tok2.has_whitespace_after ->
              let assign_shl_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignShlEq;
              } in
              _continue_gathering_op (advance p 3) assign_shl_eq
          | Token.RArrow, Token.RArrow, Token.Eq
            when not tok1.has_whitespace_after
              && not tok2.has_whitespace_after ->
              let assign_shr_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignShrEq;
              } in
              _continue_gathering_op (advance p 3) assign_shr_eq
          | Token.Eq, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let compare_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.CompareEq;
              } in
              _continue_gathering_op (advance p 2) compare_eq
          | Token.ExclMark, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let compare_not_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.CompareNotEq;
              } in
              _continue_gathering_op (advance p 2) compare_not_eq
          | Token.LArrow, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let compare_lthan_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.CompareLThanEq;
              } in
              _continue_gathering_op (advance p 2) compare_lthan_eq
          | Token.RArrow, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let compare_gthan_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.CompareGThanEq;
              } in
              _continue_gathering_op (advance p 2) compare_gthan_eq
          | Token.Plus, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_add_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignAddEq;
              } in
              _continue_gathering_op (advance p 2) assign_add_eq
          | Token.Minus, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_sub_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignSubEq;
              } in
              _continue_gathering_op (advance p 2) assign_sub_eq
          | Token.Asterisk, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_mul_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignMulEq;
              } in
              _continue_gathering_op (advance p 2) assign_mul_eq
          | Token.Slash, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_div_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignDivEq;
              } in
              _continue_gathering_op (advance p 2) assign_div_eq
          | Token.Percent, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_mod_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignModEq;
              } in
              _continue_gathering_op (advance p 2) assign_mod_eq
          | Token.Ampersand, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_and_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignAndEq;
              } in
              _continue_gathering_op (advance p 2) assign_and_eq
          | Token.Pipe, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_or_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignOrEq;
              } in
              _continue_gathering_op (advance p 2) assign_or_eq
          | Token.Circumflex, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_xor_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignXorEq;
              } in
              _continue_gathering_op (advance p 2) assign_xor_eq
          | Token.Circumflex, Token.Eq, _
            when not tok1.has_whitespace_after ->
              let assign_xor_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignXorEq;
              } in
              _continue_gathering_op (advance p 2) assign_xor_eq
          | Token.Ampersand, Token.Ampersand, _
            when not tok1.has_whitespace_after ->
              let boolean_and = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.BooleanAnd;
              } in
              _continue_gathering_op (advance p 2) boolean_and
          | Token.Pipe, Token.Pipe, _
            when not tok1.has_whitespace_after ->
              let boolean_or = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.BooleanOr;
              } in
              _continue_gathering_op (advance p 2) boolean_or
          | Token.LArrow, Token.LArrow, _
            when not tok1.has_whitespace_after ->
              let binary_shl = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.BinaryShl;
              } in
              _continue_gathering_op (advance p 2) binary_shl
          | Token.RArrow, Token.RArrow, _
            when not tok1.has_whitespace_after ->
              let binary_shr = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.BinaryShr;
              } in
              _continue_gathering_op (advance p 2) binary_shr
          | Token.LArrow, _, _ ->
              let compare_lthan = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.CompareLThan;
              } in
              _continue_gathering_op (advance p 1) compare_lthan
          | Token.RArrow, _, _ ->
              let compare_gthan = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.CompareGThan;
              } in
              _continue_gathering_op (advance p 1) compare_gthan
          | Token.Eq, _, _ ->
              let assign_eq = Op {
                pos = p.pos;
                kind = Ast.ExprOperator.AssignEq;
              } in
              _continue_gathering_op (advance p 1) assign_eq
          | _ ->
              if is_binary_operator_token tok1.kind then
                let binary_op = Op {
                  pos = p.pos;
                  kind = token_to_expr_operator tok1.kind Ast.ExprOperator.Binary;
                } in
                _continue_gathering_op (advance p 1) binary_op
              else _operand p prev
        else _operand p prev
  and _continue_gathering_op p op =
    let p, rest = _gather_data p op in
    match rest with
    | None -> p, None
    | Some x -> p, Some (op :: x)
  and _operand p prev =
    if match prev with
       | Op _ -> true
       | Ast k -> match k.kind with
                  | Ast.None -> true
                  | _ -> false then
      if (let token_kind = (peek p 0).kind in
         match token_kind with
         | Token.Identifier _
         | Token.LParen
         | Token.String _
         | Token.Symbol _ -> true
         | _ -> is_numeric_token token_kind) then
           let p, operand = parse_operands p in
           match operand with
           | None -> p, None
           | Some x ->
               let operand = Ast x in
               let p, rest = _gather_data p operand in
               match rest with
               | None -> p, None
               | Some x -> p, Some (operand :: x)
      else p, Some []
    else p, Some [] in

  _gather_data p @@ let fictional_data : Ast.t = {
                      kind = Ast.None;
                      pos = 0;
                    } in
                    let fictional_data = Ast fictional_data in
                    fictional_data

and validate_expr_data p data toplevel =
  match data with
  | [] ->
      let error : Parser_core.error = {
        kind = ExpressionIsEmpty;
        pos = p.pos;
        len = 1;
      } in
      add_error p error, false
  | _ ->
      let rec _basic_rules p data prev =
        match data with
        | [] -> p, true
        | x :: xs ->
            let cur_pos = match x with
                          | Op { pos; _ } -> pos
                          | Ast ast -> ast.pos in
            if (match x with
               | Ast _ -> false
               | Op { pos; kind } ->
                   match Ast.ExprOperator.get_group kind with
                   | Ast.ExprOperator.Prefix -> true
                   | _ -> false) &&
               (match prev with
               | Ast ast -> (match ast.kind with
                            | Ast.None -> true
                            | _ -> false)
               | Op _ -> true)
            then
              match xs with
              | [] ->
                let no_operand_after_prefix_error : Parser_core.error = {
                  kind = NoOperandAfterPrefixOperatorInExpression;
                  pos = cur_pos;
                  len = 1;
                } in
                add_error p no_operand_after_prefix_error, false
              | next :: xs ->
                  if (match next with
                     | Ast _ -> true
                     | Op { pos; kind } ->
                         match Ast.ExprOperator.get_group kind with
                         | Ast.ExprOperator.Prefix -> true
                         | _ -> false)
                  then _basic_rules p xs x
                  else
                    let expr_prefix_after_operand_error : Parser_core.error = {
                      kind = PrefixOperatorAfterAnOperand;
                      pos = cur_pos;
                      len = 1;
                    } in
                    add_error p expr_prefix_after_operand_error, false
            else if match x with | Op _ -> true | _ -> false then
              let op_kind = match x with
                            | Op { pos; kind } -> kind
                            | _ -> Util.not_reached __FILE__ __LINE__ in
              if match Ast.ExprOperator.get_group op_kind with
                 | Ast.ExprOperator.Assign when not toplevel -> true
                 | _ -> false
              then
                let assign_operator_in_non_toplevel_error : Parser_core.error = {
                  kind = AssignOperatorInNonToplevelExpression;
                  pos = cur_pos;
                  len = 1; (* FIXME: Use real operator length *)
                } in
                add_error p assign_operator_in_non_toplevel_error, false
              else if match prev with
                      | Op _ -> false
                      | Ast ast -> match ast.kind with
                                   | Ast.None -> true
                                   | _ -> false
              then
                let non_prefix_operator_at_the_beginning : Parser_core.error = {
                  kind = NonPrefixOperatorAtTheBeginningOfAnExpression;
                  pos = cur_pos;
                  len = 1; (* FIXME: Use real operator length *)
                } in
                add_error p non_prefix_operator_at_the_beginning, false
              else if match prev with Op _ -> true | Ast _ -> false then
                let two_non_prefix_operators_error : Parser_core.error = {
                  kind = TwoNonPrefixOperators;
                  pos = cur_pos;
                  len = 1; (* FIXME: Use real operator length *)
                } in
                add_error p two_non_prefix_operators_error, false
              else _basic_rules p xs x
            else if (match x with
                     | Op _ -> false
                     | _ ->
                         match prev with
                         | Op _ -> false
                         | Ast ast ->
                             match ast.kind with
                             | Ast.None -> false
                             | _ -> true)
            then
              let two_operands_error : Parser_core.error = {
                kind = TwoOperands;
                pos = cur_pos;
                len = 1;
              } in
              add_error p two_operands_error, false
            else
              _basic_rules p xs x in

      let rec _assign_operators p data has_assign =
        match data with
        | [] -> p, true
        | x :: xs ->
            match x with
            | Ast _ -> _assign_operators p xs has_assign
            | Op { pos; kind } ->
                match Ast.ExprOperator.get_group kind with
                | Ast.ExprOperator.Assign ->
                    if has_assign then
                      let two_assign_operators_error : Parser_core.error = {
                        kind = TwoAssignOperators;
                        pos = pos;
                        len = 1; (* FIXME: Real operator length *)
                      } in
                      add_error p two_assign_operators_error, false
                    else
                      _assign_operators p xs true
                | _ -> _assign_operators p xs has_assign in

      let p, success =
        let none_ast : Ast.t = {
          kind = Ast.None;
          pos = 1;
        } in
        let none_ast = Ast none_ast in
        _basic_rules p data none_ast in
      if not success then p, true
      else
        let p, success = _assign_operators p data false in
        if not success then p, true
        else
          let rec get_last xs =
            match xs with
            | [] -> None
            | [x] -> Some x
            | x :: xs -> get_last xs in
          match Option.get @@ get_last data with
          | Ast _ -> p, true
          | Op { pos; kind } ->
              let last_node_is_not_an_operand_error : Parser_core.error = {
                kind = LastNodeIsNotAnOperandInExpression;
                pos = pos;
                len = 1;
              } in
              add_error p last_node_is_not_an_operand_error, false

and pratt_parse_expr data min_bp =
  (* Get prefix expression if "stepped" on it *)
  let _prefix_expr data =
    let rec _gather_operators_and_operand data =
      match data with
      | [] -> Util.not_reached __FILE__ __LINE__
      | [x] ->
          (* This case is already checked in validate_expr_data, so no need to do it twice *)
          (match x with
          | Op _ -> Util.not_reached __FILE__ __LINE__
          | Ast ast -> [], ast, [])
      | x :: xs ->
          match x with
          | Ast ast -> [], ast, xs
          | Op { pos; kind } ->
              let operators, ast, data = _gather_operators_and_operand xs in
              let operators = operators @ [kind] in
              operators, ast, data in
    match data with
    | [] -> Util.not_reached __FILE__ __LINE__
    | x :: xs ->
        match x with
        | Ast _ -> None, data
        | Op { pos; kind } ->
            match Ast.ExprOperator.get_group kind with
            | Ast.ExprOperator.Prefix ->
                let operators, operand, data = _gather_operators_and_operand data in
                let prefix_expr_ast : Ast.t = {
                  kind = Ast.PrefixExpr { operators = operators; operand = operand };
                  pos = operand.pos;
                } in
                Some prefix_expr_ast, data
            | _ -> None, data in

  (* Pratt-parsing part *)
  let rec _pratt lhs data =
    match data with
    | [] -> lhs, []
    | y :: ys ->
        let op_kind =
          match y with
          | Op { pos; kind } -> kind
          | _ -> Util.not_reached __FILE__ __LINE__ in
        let l_bp, r_bp = get_binding_power op_kind in
        if l_bp < min_bp then
          lhs, data
        else
          let rhs, data = pratt_parse_expr ys r_bp in
          let lhs : Ast.t = {
            kind = Ast.Expr { lhs = lhs; rhs = rhs; operator = op_kind };
            pos = lhs.pos;
          } in
          _pratt lhs data in

  match data with
  | [] ->
      let none_ast : Ast.t = {
        kind = Ast.None;
        pos = -1;
      } in
      none_ast, []
  | x :: xs ->
      let lhs, xs =
        let pretend_lhs, data = _prefix_expr data in
        match pretend_lhs with
        | Some y -> y, data
        | None ->
            match x with
            | Ast ast -> ast, xs
            | _ -> Util.not_reached __FILE__ __LINE__ in
      _pratt lhs xs

and parse_expr p toplevel =
  let p, data = parse_expr_data p in
  match data with
  | None -> p, None
  | Some data ->
      let p, is_valid = validate_expr_data p data toplevel in
      if not is_valid then p, None
      else
        let expr, _ = pratt_parse_expr data 0 in
        p, Some expr

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
      | _ -> advance (add_error_unexpected p @@ Token.Semicolon) 1, None
