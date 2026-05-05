type t = {
  src: string;
  pos: int;
  line: int;
  llp: int;
  ch: char option;
}

val create : string -> t
val tokenize : t -> t * Token.t list * Token.t list
