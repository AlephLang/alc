include Parser_core
open Parser_top

let parse parser =
  let rec _toplevels p =
    if p.pos >= List.length p.tokens then p, []
    else
      let p, toplevel = parse_top p in
      let p, remaining = _toplevels p in
      match toplevel with
      | None -> p, remaining
      | Some x -> p, x :: remaining in

  let parser, toplevels = _toplevels parser in
  let root : Ast.t = {
    kind = Ast.Root { toplevel_statements = toplevels };
    pos = 0;
  } in
  parser, root
