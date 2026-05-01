open Alcotest

let int_to_bin_str x expected () =
  let got = (Printf.sprintf "%a" Alc.Util.pp_binint x) in
  check string "" expected got

let int_to_bin_str_suite =
  [ "123", `Quick, int_to_bin_str 123 "1111011"
  ; "-123", `Quick, int_to_bin_str (-123)
     "111111111111111111111111111111111111111111111111111111110000101"
  ]

let isspace_check range expected () =
  let f, l = range in
  let result = ref true in
  for i = f to l do
    result := Alc.Util.is_space (char_of_int i) = expected
  done;
  check bool "" !result true

let isspace_suite =
  [ "control codes 1", `Quick, isspace_check (0, 8) false
  ; "tab", `Quick, isspace_check (9, 9) true
  ; "whitespaces", `Quick, isspace_check (10, 13) true
  ; "control codes 2", `Quick, isspace_check (14, 31) false
  ; "space", `Quick, isspace_check (32, 32) true
  ; "punctuation 1", `Quick, isspace_check (33, 47) false
  ; "digits", `Quick, isspace_check (48, 57) false
  ; "punctuation 2", `Quick, isspace_check (58, 64) false
  ; "first 6 uppercase ASCII letters", `Quick, isspace_check (65, 70) false
  ; "remaining uppercase ASCII letters", `Quick, isspace_check (71, 90) false
  ; "punctuation 3", `Quick, isspace_check (91, 96) false
  ; "first 6 lowercase ASCII letters", `Quick, isspace_check (97, 102) false
  ; "remaining lowercase ASCII letters", `Quick, isspace_check (103, 122) false
  ; "punctuation 4", `Quick, isspace_check (123, 126) false
  ; "backspace", `Quick, isspace_check (127, 127) false
  ]
