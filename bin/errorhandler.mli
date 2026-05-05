type t = {
  src: string list;
  tokens: Alc.Token.t list;
  lnoffset: int;
}

val create : string -> t
val set_tokens : t -> Alc.Token.t list -> t
val handle_lexer_errors : t -> string -> Alc.Token.t list -> unit
val handle_parser_errors : t -> string -> Alc.Parser.error list -> unit
