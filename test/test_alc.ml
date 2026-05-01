open Alcotest

let () =
  Alcotest.run "Alc" [ "Util (int to bin)", Test_util.int_to_bin_str_suite
                     ; "Util (isspace)", Test_util.isspace_suite
                     ; "Token", Test_token.suite ]
