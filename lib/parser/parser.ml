include Parser_core
open Parser_top

let rec parse_toplevels p =
  if p.pos >= List.length p.tokens then p, []
  else
    let p, toplevel = parse_top p in
    let p, remaining = parse_toplevels p in
    match toplevel with
    | None -> p, remaining
    | Some x -> p, x :: remaining

let parse parser =
  let parser, toplevels = parse_toplevels parser in
  let root : Ast.t = {
    kind = Ast.Root { toplevel_statements = toplevels };
    pos = 0;
  } in
  parser, root
