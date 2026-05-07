open Parser_core

val parse_type_raw : t -> t * Ast.t option
val parse_type : t -> t * Ast.t option
