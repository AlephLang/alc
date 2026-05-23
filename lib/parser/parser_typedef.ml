open Parser_core
open Parser_type
open Parser_generic_placeholder_type_list
open Parser_attributes
open Util

let parse_typedef p =
  let pos = p.pos in

  (* using ... name ... = type ; *)
  (* ^~~~~                       *)
  let rec _using p =
    match (peek p 0).kind with
    | Token.Identifier "using" -> advance p 1 |> _attribs
    | Token.Identifier _ -> advance (add_error_value p "using") 1, None
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "using") 1, None

  (* using [[attribs...]] name ... = type ; *)
  (*       ^~~~~~~~~~~~~~                   *)
  and _attribs p =
    match (peek p 0).kind with
    | Token.LBrack ->
        let p, attribs = parse_attribute_list p in
        (match attribs with
        | None -> p, None
        | Some _ -> _name p attribs)
    | _ -> _name p None

  (* using ... name ... = type ; *)
  (*           ^~~~              *)
  and _name p attribs_opt =
    match (peek p 0).kind with
    | Token.Identifier value -> _generic_placeholder_type_list (advance p 1) attribs_opt value
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p @@ Token.Identifier "") 1, None

  (* using ... name <types...> = type ; *)
  (*                ^~~~~~~~~~          *)
  and _generic_placeholder_type_list p attribs_opt name =
    match (peek p 0).kind with
    | Token.LArrow ->
        let p, generic_placeholder_type_list = parse_generic_placeholder_type_list p in
        (match generic_placeholder_type_list with
        | None -> p, None
        | Some _ -> _eq p attribs_opt name generic_placeholder_type_list)
    | _ -> _eq p attribs_opt name None

  (* using ... name <types...> = type ; *)
  (*                           ^        *)
  and _eq p attribs_opt name gptl_opt =
    match (peek p 0).kind with
    | Token.Eq -> _type (advance p 1) attribs_opt name gptl_opt
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Eq) 1, None

  (* using name ... <types...> = type ; *)
  (*                             ^~~~   *)
  and _type p attribs_opt name gptl_opt =
    let p, type_ = parse_type p in
    match type_ with
    | None -> p, None
    | Some x -> _semicolon p attribs_opt name gptl_opt x

  (* using name <types...> = type ; *)
  (*                              ^ *)
  and _semicolon p attribs_opt name gptl_opt aliased_type =
    match (peek p 0).kind with
    | Token.Semicolon ->
        let typedef_ast : Ast.t = {
          kind = Ast.TypeDef { name = name
                             ; generic_placeholder_type_list = gptl_opt
                             ; aliased_type = aliased_type
                             ; attribute_list = attribs_opt };
          pos = pos;
        } in
        advance p 1, Some typedef_ast
    | Token.Eof -> add_error_eof p, None
    | _ -> advance (add_error_unexpected p Token.Semicolon) 1, None in

  _using p
