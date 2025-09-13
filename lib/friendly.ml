let safe_chars = [
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  "abcdefghijklmnopqrstuvwxyz";
  "0123456789";
  "_-"
] |> String.concat ""

let is_safe_char c =
  String.contains safe_chars c

let whitespace = [' '; '\n'; '\r'; '\t']

let is_whitespace c =
  match List.find_opt (Char.equal c) whitespace with
  | Some _ -> true
  | None -> false

let safe_char c =
  if is_safe_char c then
    c
  else if is_whitespace c then
    '-'
  else
    ' '

let dohickey_name input =
  let open String in
  input
  |> lowercase_ascii
  |> map safe_char
  |> split_on_char ' '
  |> List.filter (fun s -> String.length s <> 0)
  |> concat ""
