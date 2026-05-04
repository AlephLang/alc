type t =
  | Reset
  | Color_Black
  | Color_Red
  | Color_Green
  | Color_Yellow
  | Color_Blue
  | Color_Magenta
  | Color_Cyan
  | Color_White
  | Color_Default
  | Graphics_Bold
  | Graphics_Dim
  | Graphics_Italic
  | Graphics_Underline
  | Graphics_Blink
  | Graphics_Inverse
  | Graphics_Hidden
  | Graphics_Strikethrough

let to_ansi modes =
  let rec to_ansi_list modes =
    match modes with
    | [] -> ""
    | x :: xs ->
        let value =
          match x with
          | Reset -> "\027[0m"
          | Color_Black -> "\027[30m"
          | Color_Red -> "\027[31m"
          | Color_Green -> "\027[32m"
          | Color_Yellow -> "\027[33m"
          | Color_Blue -> "\027[34m"
          | Color_Magenta -> "\027[35m"
          | Color_Cyan -> "\027[36m"
          | Color_White -> "\027[37m"
          | Color_Default -> "\027[39m"
          | Graphics_Bold -> "\027[1m"
          | Graphics_Dim -> "\027[2m"
          | Graphics_Italic -> "\027[3m"
          | Graphics_Underline -> "\027[4m"
          | Graphics_Blink -> "\027[5m"
          | Graphics_Inverse -> "\027[7m"
          | Graphics_Hidden -> "\027[8m"
          | Graphics_Strikethrough -> "\027[9m" in
        value ^ to_ansi_list xs in

  match modes with
  | [] -> "\027[0m"
  | _ -> "\027[0m" ^ to_ansi_list modes

