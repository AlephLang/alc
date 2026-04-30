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
