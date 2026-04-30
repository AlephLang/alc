open Alcotest

let int_to_bin_str x expected () =
  let got = (Printf.sprintf "%a" Alc.Util.pp_binint x) in
  check string "same strings" expected got

let suite =
  [ "int to binary string (123)", `Quick, int_to_bin_str 123 "1111011"
  ; "int to binary string (-123)", `Quick, int_to_bin_str (-123)
     "111111111111111111111111111111111111111111111111111111110000101"
  ]
