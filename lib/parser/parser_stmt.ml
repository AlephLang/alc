open Parser_core
open Parser_return
open Parser_enum
open Parser_expr
open Parser_attributes
open Parser_goto
open Parser_label
open Parser_typedef
open Parser_misc

let __parse_defer : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_for : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_while : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_dowhile : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_loop : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_foreach : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_struct : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_union : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_decldef : (t -> Ast.t option -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_if : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_switch : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)

let rec parse_stmt p =
  let tok1 = peek p 0 in
  match tok1.kind with
  | Token.Semicolon ->
      let none_ast : Ast.t = {
        kind = Ast.None;
        pos = p.pos;
      } in
      advance p 1, Some none_ast
  | Token.Identifier value ->
      (match value with
      | "return" -> parse_return p
      | "using" -> parse_typedef p
      | "defer" -> !__parse_defer p
      | "for" -> !__parse_for p
      | "while" -> !__parse_while p
      | "do" -> !__parse_dowhile p
      | "loop" -> !__parse_loop p
      | "foreach" -> !__parse_foreach p
      | "continue" ->
          let pos, p = p.pos, advance p 1 in
          (match (peek p 0).kind with
          | Token.Semicolon ->
              let continue_ast : Ast.t = {
                kind = Ast.StmtContinue;
                pos = pos;
              } in
              advance p 1, Some continue_ast
          | Token.Eof -> add_error_eof p, None
          | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None)
      | "break" ->
          let pos, p = p.pos, advance p 1 in
          (match (peek p 0).kind with
          | Token.Semicolon ->
              let break_ast : Ast.t = {
                kind = Ast.StmtBreak;
                pos = pos;
              } in
              advance p 1, Some break_ast
          | Token.Eof -> add_error_eof p, None
          | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None)
      | "fallthrough" ->
          let pos, p = p.pos, advance p 1 in
          (match (peek p 0).kind with
          | Token.Semicolon ->
              let fallthrough_ast : Ast.t = {
                kind = Ast.StmtFallthrough;
                pos = pos;
              } in
              advance p 1, Some fallthrough_ast
          | Token.Eof -> add_error_eof p, None
          | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None)
      | "struct" -> !__parse_struct p
      | "enum" -> parse_enum p
      | "union" -> !__parse_union p
      | "func" -> !__parse_decldef p None
      | "goto" -> parse_goto p
      | "if" -> !__parse_if p
      | "switch" -> !__parse_switch p
      | _ ->
          let tok2, tok3, tok4 = peek p 1, peek p 2, peek p 3 in
          match tok2.kind, tok3.kind, tok4.kind with
          | Token.Colon, Token.Colon, Token.LArrow
          | Token.Colon, Token.Colon, Token.LParen -> !__parse_decldef p None
          | Token.Colon, Token.Colon, _ -> parse_stmt_expr p
          | Token.Colon, _, _ -> !__parse_decldef p None
          | _ ->
              if is_qualifier value then !__parse_decldef p None
              else parse_stmt_expr p)
  | Token.At -> parse_label p
  | Token.LCBrack -> parse_stmt_block p
  | Token.LBrack ->
      let p, attribs = parse_attribute_list p in
      (match attribs with
      | None -> p, None
      | Some _ -> !__parse_decldef p attribs)
  | Token.Eof -> add_error_eof p, None
  | _ -> parse_stmt_expr p

and parse_stmt_block p =
  let rec _stmts p =
    match (peek p 0).kind with
    | Token.RCBrack -> advance p 1, Some []
    | Token.Eof -> add_error_eof p, None
    | _ ->
        let p, stmt = parse_stmt p in
        match stmt with
        | None -> p, None
        | Some x ->
            let p, rest = _stmts p in
            match rest with
            | None -> p, None
            | Some y -> p, Some (x :: y) in

  match (peek p 0).kind with
  | Token.LCBrack ->
      let pos = p.pos in
      let p, stmts = advance p 1 |> _stmts in
      (match stmts with
      | None -> p, None
      | Some x ->
          let stmt_block_ast : Ast.t = {
            kind = Ast.StmtBlock { statements = x };
            pos = pos;
          } in
          p, Some stmt_block_ast)
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p Token.LCBrack) 1, None
