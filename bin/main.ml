let () =
  let args_num = Array.length Sys.argv - 1 in
  if args_num = 0 then
    Printf.eprintf "No sources were provided.\n"
  else
    for i = 1 to args_num do
      let file = open_in Sys.argv.(i) in
      try
        (* Read from file *)
        let src = In_channel.input_all file in
        close_in file;

        (* Tokenize *)
        let _, tokens = Alc.Lexer.tokenize @@ Alc.Lexer.create src in
        let tokens_str = ref "" in
        for j = 0 to List.length tokens - 1 do
          tokens_str := !tokens_str ^ Printf.sprintf "(%i) Token %a\n" j Alc.Token.pp @@ List.nth tokens j
        done;
        print_endline !tokens_str
      with e ->
        close_in_noerr file;
        raise e
    done;
