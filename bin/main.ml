let () =
  let args_num = Array.length Sys.argv - 1 in
  if args_num = 0 then begin
    Printf.eprintf "No sources were provided.\n";
    exit @@ -1
  end;

  for i = 1 to args_num do
    let filename = Sys.argv.(i) in
    let file = open_in filename in
    let closed = ref false in
    try
      (* Read from file *)
      let src = In_channel.input_all file in
      close_in file;
      closed := true;

      (* Create error handler *)
      let errorhandler = Errorhandler.create src in

      (* Tokenize *)
      let _, tokens, lexer_errors = Alc.Lexer.tokenize @@ Alc.Lexer.create src in
      let tokens_str = ref "" in
      for j = 0 to List.length tokens - 1 do
        tokens_str := !tokens_str
                    ^ Printf.sprintf "(%i) Token %a\n" j Alc.Token.pp
                   @@ List.nth tokens j
      done;
      print_endline !tokens_str;

      if List.length lexer_errors > 0 then begin
        Errorhandler.handle_lexer_errors errorhandler filename lexer_errors;
        exit @@ -2
      end;
      let errorhandler = Errorhandler.set_tokens errorhandler tokens in

      (* Parse *)
      let parser, ast = Alc.Parser.parse @@ Alc.Parser.create tokens in
      print_endline @@ Alc.Ast.show ast;

      if List.length parser.errors > 0 then begin
        Errorhandler.handle_parser_errors errorhandler filename parser.errors;
        exit @@ -3
      end;
    with e ->
      if not !closed then close_in_noerr file
      else ();
      raise e
  done;
