let pp_binint ppf x =
  let open Printf in
  if x = 0 then "0"
  else
    let rec getstr_bin x =
      if x = 0 then ""
      else
        let chr = string_of_int (x land 1) in
        getstr_bin (x lsr 1) ^ chr in
    getstr_bin x

let is_space x =
  let x = int_of_char x in
  (x >= 9 && x <= 13) || x = 32

let preproc_str s =
  let b = ref "" in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\n' -> b := !b ^ "\\n"
    | '\r' -> b := !b ^ "\\r"
    | '\t' -> b := !b ^ "\\t"
    | _ -> b := !b ^ String.make 1 s.[i]
  done;
  !b
