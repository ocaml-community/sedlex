let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

let rec token buf =
  match%sedlex buf with
    | number ->
        Printf.printf "Number %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | id_start, Star id_continue ->
        Printf.printf "Ident %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | Plus xml_blank -> token buf
    | Plus (Chars "+*-/") ->
        Printf.printf "Op %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | eof -> print_endline "EOF"
    | any ->
        Printf.printf "Any %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | _ -> assert false

let utf16_of_utf8 ?(endian = Sedlexing.Utf16.Big_endian) s =
  let b = Buffer.create (String.length s * 4) in
  let rec loop pos =
    if pos >= String.length s then ()
    else (
      let c = String.get_utf_8_uchar s pos in
      let u = Uchar.utf_decode_uchar c in
      (match endian with
        | Big_endian -> Buffer.add_utf_16be_uchar b u
        | Little_endian -> Buffer.add_utf_16le_uchar b u);
      loop (pos + Uchar.utf_decode_length c))
  in
  loop 0;
  Buffer.contents b

let remove_last s n = String.sub s 0 (String.length s - n)

let gen_from_string s =
  let i = ref 0 in
  fun () ->
    if !i >= String.length s then None
    else (
      let c = String.get s !i in
      incr i;
      Some c)

let channel_from_string s =
  let name, oc = Filename.open_temp_file "" "" in
  output_string oc s;
  close_out oc;
  open_in_bin name

let test_latin s f =
  print_endline "== from_string ==";
  (try f (Sedlexing.Latin1.from_string s)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_gen ==";
  (try f (Sedlexing.Latin1.from_gen (gen_from_string s))
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_channel ==";
  try f (Sedlexing.Latin1.from_channel (channel_from_string s))
  with Sedlexing.MalFormed -> print_endline "MalFormed"

let test_utf8 s f =
  print_endline "== from_string ==";
  (try f (Sedlexing.Utf8.from_string s)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_gen ==";
  (try f (Sedlexing.Utf8.from_gen (gen_from_string s))
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_channel ==";
  try f (Sedlexing.Utf8.from_channel (channel_from_string s))
  with Sedlexing.MalFormed -> print_endline "MalFormed"

let test_utf16 s bo f =
  print_endline "== from_string ==";
  (try f (Sedlexing.Utf16.from_string s bo)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_gen ==";
  (try f (Sedlexing.Utf16.from_gen (gen_from_string s) bo)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_channel ==";
  try f (Sedlexing.Utf16.from_channel (channel_from_string s) bo)
  with Sedlexing.MalFormed -> print_endline "MalFormed"

let%expect_test "latin1" =
  let s = "asas 123 + 2asd" in
  test_latin s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_gen ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_channel ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF |}];
  let s = "asas 123 + 2\129" in
  test_latin s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Ident asas
    Number 123
    Op +
    Number 2
    Any 
    EOF
    == from_gen ==
    Ident asas
    Number 123
    Op +
    Number 2
    Any 
    EOF
    == from_channel ==
    Ident asas
    Number 123
    Op +
    Number 2
    Any 
    EOF |}]

let%expect_test "utf8" =
  let s = "asas 123 + 2asd" in
  test_utf8 s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_gen ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_channel ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF |}];
  let s = "asas 123 + 2\129" in
  test_utf8 s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    MalFormed |}]

let%expect_test "utf16" =
  let bo = None in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_gen ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_channel ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Ident asas
    Number 123
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Number 12
    Ident asd12
    Any 🛳
    EOF
    == from_gen ==
    Number 12
    Ident asd12
    Any 🛳
    EOF
    == from_channel ==
    Number 12
    Ident asd12
    Any 🛳
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Number 12
    Ident asd12
    EOF
    == from_gen ==
    Number 12
    Ident asd12
    EOF
    == from_channel ==
    Number 12
    Ident asd12
    EOF |}]

let%expect_test "utf16-be" =
  let endian = Sedlexing.Utf16.Big_endian in
  let utf16_of_utf8 = utf16_of_utf8 ~endian in
  let bo = Some endian in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_gen ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_channel ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Ident asas
    Number 123
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Number 12
    Ident asd12
    Any 🛳
    EOF
    == from_gen ==
    Number 12
    Ident asd12
    Any 🛳
    EOF
    == from_channel ==
    Number 12
    Ident asd12
    Any 🛳
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Number 12
    Ident asd12
    EOF
    == from_gen ==
    Number 12
    Ident asd12
    EOF
    == from_channel ==
    Number 12
    Ident asd12
    EOF |}]

let%expect_test "utf16-le" =
  let endian = Sedlexing.Utf16.Little_endian in
  let utf16_of_utf8 = utf16_of_utf8 ~endian in
  let bo = Some endian in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_gen ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF
    == from_channel ==
    Ident asas
    Number 123
    Op +
    Number 2
    Ident asd
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Ident asas
    Number 123
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Number 12
    Ident asd12
    Any 🛳
    EOF
    == from_gen ==
    Number 12
    Ident asd12
    Any 🛳
    EOF
    == from_channel ==
    Number 12
    Ident asd12
    Any 🛳
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    MalFormed
    == from_gen ==
    MalFormed
    == from_channel ==
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    Number 12
    Ident asd12
    EOF
    == from_gen ==
    Number 12
    Ident asd12
    EOF
    == from_channel ==
    Number 12
    Ident asd12
    EOF |}]
