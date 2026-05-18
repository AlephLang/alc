open Parser_core

val __parse_type : (t -> t * Ast.t option) ref

val parse_generic_type_list : t -> t * Ast.t option
