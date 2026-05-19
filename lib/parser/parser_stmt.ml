open Parser_core
open Parser_return
open Parser_enum

let __parse_defer : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_while : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_dowhile : (t -> t * Ast.t option) ref =
  ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_struct : (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)
let __parse_union: (t -> t * Ast.t option) ref = ref (fun _ -> Util.not_reached __FILE__ __LINE__)

let rec parse_stmt p =
  match (peek p 0).kind with
  | Token.Semicolon ->
      let none_ast : Ast.t = {
        kind = Ast.None;
        pos = p.pos;
      } in
      advance p 1, Some none_ast
  | Token.Identifier value ->
      (match value with
      | "return" -> parse_return p
      | "defer" -> !__parse_defer p
      | "while" -> !__parse_while p
      | "do" -> !__parse_dowhile p
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
      | _ -> advance p 1, None)
  | Token.LCBrack -> parse_stmt_block p
  | Token.Eof -> add_error_eof p, None
  | _ -> advance (add_error_unexpected p @@ Token.Error "") 1, None

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
