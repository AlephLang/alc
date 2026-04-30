open Alcotest

let () =
  Alcotest.run "Alc" [ "Util", Test_util.suite;
                       "Token", Test_token.suite ]
