open Parser_core

val parse_expr : t -> bool -> t * Ast.t option
val parse_stmt_expr : t -> t * Ast.t option

val __parse_generic_type_list : (t -> t * Ast.t option) ref
val __parse_type : (t -> t * Ast.t option) ref
val __parse_type_raw : (t -> t * Ast.t option) ref
val __parse_initlist : (t -> t * Ast.t option) ref
