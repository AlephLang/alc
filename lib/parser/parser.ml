include Parser_core
open Parser_top

let rec parse_toplevels p =
  if p.pos >= List.length p.tokens then p, []
  else
    let p, toplevel = parse_top p in
    let p, rest = parse_toplevels p in
    match toplevel with
    | None -> p, rest
    | Some x -> p, x :: rest

let parse parser =
  (* Set function pointers(?) in 'Parser_expr' *)
  Parser_expr.__parse_generic_type_list := Parser_generic_type_list.parse_generic_type_list;
  Parser_expr.__parse_type := Parser_type.parse_type;
  Parser_expr.__parse_type_raw := Parser_type.parse_type_raw;
  Parser_expr.__parse_initlist := Parser_initlist.parse_initlist;

  (* For 'Parser_decldef' *)
  Parser_decldef.__parse_type := Parser_type.parse_type;
  Parser_decldef.__parse_vardecl := Parser_vardecl.parse_vardecl;
  Parser_decldef.__parse_vardef := Parser_vardef.parse_vardef;
  Parser_decldef.__parse_function := Parser_function.parse_function;
  Parser_decldef.__parse_generic_function := Parser_generic_function.parse_generic_function;

  (* For 'Parser_generic_type_list' *)
  Parser_generic_type_list.__parse_type := Parser_type.parse_type;

  (* For 'Parser_stmt' *)
  Parser_stmt.__parse_defer := Parser_defer.parse_defer;

  let parser, toplevels = parse_toplevels parser in
  let root : Ast.t = {
    kind = Ast.Root { toplevel_statements = toplevels };
    pos = 0;
  } in
  parser, root
