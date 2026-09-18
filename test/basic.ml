let () = set_binary_mode_out stdout true
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

let hex_digit =
  let digit = [%sedlex.regexp? '0' .. '9'] in
  let hex_letter = [%sedlex.regexp? 'a' .. 'f' | 'A' .. 'F'] in
  [%sedlex.regexp? digit | hex_letter]

let print_pos buf =
  let f { Lexing.pos_lnum; pos_bol; pos_cnum; _ } =
    Printf.sprintf "line=%d:bol=%d:cnum=%d" pos_lnum pos_bol pos_cnum
  in
  let f ~prefix (startp, endp) =
    Printf.printf "%s pos: [%s;%s]\n" prefix (f startp) (f endp)
  in
  f ~prefix:"code point" (Sedlexing.lexing_positions buf);
  f ~prefix:"bytes" (Sedlexing.lexing_bytes_positions buf)

let rec token buf =
  match%sedlex buf with
    | number ->
        print_pos buf;
        Printf.printf "Number %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | id_start, Star id_continue ->
        print_pos buf;
        Printf.printf "Ident %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | Plus xml_blank -> token buf
    | Plus (Chars "+*-/") ->
        print_pos buf;
        Printf.printf "Op %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | eof ->
        print_pos buf;
        print_endline "EOF"
    | any ->
        print_pos buf;
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
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    EOF |}];
  let s = "asas 123 + 2\129" in
  test_latin s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    Any 
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    Any 
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    Any 
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    EOF |}]

let%expect_test "utf8" =
  let s = {|as🎉as 123 + 2asd|} in
  test_utf8 s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=16]
    Number 2
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=19]
    Ident asd
    code point pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=19;line=1:bol=0:cnum=19]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=16]
    Number 2
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=19]
    Ident asd
    code point pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=19;line=1:bol=0:cnum=19]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=16]
    Number 2
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=19]
    Ident asd
    code point pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=19;line=1:bol=0:cnum=19]
    EOF |}];
  let s = {|as🎉as 123 + 2|} ^ "\129" in
  test_utf8 s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    MalFormed |}]

let%expect_test "utf16" =
  let bo = None in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
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
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
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
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF |}]

let%expect_test "utf8 surrogate rejection" =
  (* UTF-16 surrogates (U+D800..U+DFFF) must be rejected as invalid UTF-8 *)
  let test s =
    try
      let lb = Sedlexing.Utf8.from_string s in
      ignore (Sedlexing.__private__next_int lb);
      Printf.printf "accepted (BUG)\n"
    with Sedlexing.MalFormed -> Printf.printf "rejected\n"
  in
  (* U+D800: first high surrogate *)
  test "\xED\xA0\x80";
  (* U+DF01: low surrogate *)
  test "\xED\xBC\x81";
  (* U+DFFF: last low surrogate *)
  test "\xED\xBF\xBF";
  [%expect {|
    rejected
    rejected
    rejected |}]

let%expect_test "nested_let_regexp" =
  let int_lit =
    let digit = [%sedlex.regexp? '0' .. '9'] in
    [%sedlex.regexp? Plus digit]
  in
  let buf = Sedlexing.Utf8.from_string "123abc" in
  let rec loop () =
    match%sedlex buf with
      | int_lit ->
          Printf.printf "Int: %s\n" (Sedlexing.Utf8.lexeme buf);
          loop ()
      | Plus 'a' .. 'z' ->
          Printf.printf "Word: %s\n" (Sedlexing.Utf8.lexeme buf);
          loop ()
      | eof -> Printf.printf "EOF\n"
      | _ -> assert false
  in
  loop ();
  [%expect {|
    Int: 123
    Word: abc
    EOF |}]

let%expect_test "nested_let_regexp_toplevel" =
  let buf = Sedlexing.Utf8.from_string "0xDEAD rest" in
  let rec loop () =
    match%sedlex buf with
      | "0x", Plus hex_digit ->
          Printf.printf "Hex: %s\n" (Sedlexing.Utf8.lexeme buf);
          loop ()
      | Plus 'a' .. 'z' ->
          Printf.printf "Word: %s\n" (Sedlexing.Utf8.lexeme buf);
          loop ()
      | ' ' -> loop ()
      | eof -> Printf.printf "EOF\n"
      | _ -> assert false
  in
  loop ();
  [%expect {|
    Hex: 0xDEAD
    Word: rest
    EOF |}]

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let%expect_test "as_bindings" =
  (* Test 1: simple binding in middle of sequence *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | 'a', ('b' as x), 'c' ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=b |}];
  (* Test 2: multiple bindings *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | ('a' as x), ('b' as y), 'c' ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=a y=b |}];
  (* Test 3: binding with named regexp *)
  let buf = Sedlexing.Utf8.from_string "123z" in
  (match%sedlex buf with
    | number, (letter as x) ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=z |}];
  (* Test 4: whole-match binding *)
  let buf = Sedlexing.Utf8.from_string "hello" in
  (match%sedlex buf with
    | Plus 'a' .. 'z' as x ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=hello |}];
  (* Test 5: multi-char UTF-8 *)
  let buf = Sedlexing.Utf8.from_string "a\xC3\xA9b" in
  (match%sedlex buf with
    | 'a', (any as x), 'b' ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=é |}];
  (* Test 6: variable-length named segment *)
  let buf = Sedlexing.Utf8.from_string {|"hello"|} in
  (match%sedlex buf with
    | '"', (Star (Compl '"') as content), '"' ->
        Printf.printf "content=%s\n" (Sedlexing.Utf8.of_submatch content)
    | _ -> assert false);
  [%expect {| content=hello |}];
  (* Test 7: as binding wrapping an alternation *)
  let buf = Sedlexing.Utf8.from_string "xb" in
  (match%sedlex buf with
    | 'x', (('a' | 'b') as x) ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=b |}];
  (* Test 8: as binding in both branches of or-pattern *)
  let buf = Sedlexing.Utf8.from_string "123" in
  (match%sedlex buf with
    | (number as x) | (Plus letter as x) ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=123 |}];
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | (number as x) | (Plus letter as x) ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=abc |}];
  (* Test 9: as binding inside or, in a sequence *)
  let buf = Sedlexing.Utf8.from_string "<42>" in
  (match%sedlex buf with
    | '<', ((number as x) | (Plus letter as x)), '>' ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=42 |}];
  let buf = Sedlexing.Utf8.from_string "<hello>" in
  (match%sedlex buf with
    | '<', ((number as x) | (Plus letter as x)), '>' ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=hello |}];
  (* Test 10: or-pattern with shared prefix requiring discriminator tags *)
  let buf = Sedlexing.Utf8.from_string "abcdef" in
  (match%sedlex buf with
    | ("abc" as x), "def" | "a", ("bcd" as x), "ey" ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=abc |}];
  let buf = Sedlexing.Utf8.from_string "abcdey" in
  (match%sedlex buf with
    | ("abc" as x), "def" | "a", ("bcd" as x), "ey" ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=bcd |}];
  (* Test 10b: 3-way or-pattern reuses single disc cell *)
  let buf = Sedlexing.Utf8.from_string "abcd" in
  (match%sedlex buf with
    | ("ab" as x), "cd" | ("a" as x), "bce" | ("abc" as x), "df" ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=ab |}];
  let buf = Sedlexing.Utf8.from_string "abce" in
  (match%sedlex buf with
    | ("ab" as x), "cd" | ("a" as x), "bce" | ("abc" as x), "df" ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=a |}];
  let buf = Sedlexing.Utf8.from_string "abcdf" in
  (match%sedlex buf with
    | ("ab" as x), "cd" | ("a" as x), "bce" | ("abc" as x), "df" ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=abc |}];
  (* Test 10c: or-pattern with inner or + extra binding without disc *)
  let buf = Sedlexing.Utf8.from_string "aef" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y) | ("cd" as x), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=a y=ef |}];
  let buf = Sedlexing.Utf8.from_string "bef" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y) | ("cd" as x), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=b y=ef |}];
  let buf = Sedlexing.Utf8.from_string "cdgh" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y) | ("cd" as x), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=cd y=gh |}];
  (* Test 10d: nested or-patterns on both sides *)
  let buf = Sedlexing.Utf8.from_string "aef" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y)
    | (("c" as x) | ("d" as x)), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=a y=ef |}];
  let buf = Sedlexing.Utf8.from_string "bef" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y)
    | (("c" as x) | ("d" as x)), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=b y=ef |}];
  let buf = Sedlexing.Utf8.from_string "cgh" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y)
    | (("c" as x) | ("d" as x)), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=c y=gh |}];
  let buf = Sedlexing.Utf8.from_string "dgh" in
  (match%sedlex buf with
    | (("a" as x) | ("b" as x)), ("ef" as y)
    | (("c" as x) | ("d" as x)), ("gh" as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=d y=gh |}];
  (* Test 11: Set_prev with backtracking (Opt at end) *)
  let buf = Sedlexing.Utf8.from_string "aabba" in
  (match%sedlex buf with
    | (Plus 'a' as x), ((Plus 'b', Opt 'a') as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=aa y=bba |}];
  let buf = Sedlexing.Utf8.from_string "aabb" in
  (match%sedlex buf with
    | (Plus 'a' as x), ((Plus 'b', Opt 'a') as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=aa y=bb |}];
  let buf = Sedlexing.Utf8.from_string "aba" in
  (match%sedlex buf with
    | (Plus 'a' as x), ((Plus 'b', Opt 'a') as y) ->
        Printf.printf "x=%s y=%s\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y)
    | _ -> assert false);
  [%expect {| x=a y=ba |}]

let num_mem buf = Sedlexing.__private__num_mem_cells buf

let%expect_test "as_bindings_num_mem_cells" =
  (* No bindings: 0 cells *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | "abc" -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Single binding in tuple: 0 cells (prefix + suffix known) *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | 'a', ('b' as _x), 'c' -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Two bindings in tuple: 0 cells (all offsets known) *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | ('a' as _x), ('b' as _y), 'c' ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Whole-match binding: 0 cells (Start_plus 0, End_minus 0) *)
  let buf = Sedlexing.Utf8.from_string "hello" in
  (match%sedlex buf with
    | Plus 'a' .. 'z' as _x -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* as wrapping alternation in tuple: 0 cells (prefix=1, suffix=0 known) *)
  let buf = Sedlexing.Utf8.from_string "xb" in
  (match%sedlex buf with
    | 'x', (('a' | 'b') as _x) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Or-pattern with identical offsets: 0 cells (disc elided) *)
  let buf = Sedlexing.Utf8.from_string "123" in
  (match%sedlex buf with
    | (number as _x) | (Plus letter as _x) ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Or-pattern with different offsets: positions are known, so only the
     discriminator cell is needed (conflict-free: a branch's discriminator
     fires just before its final node, so its holder never survives into a
     state where the other branch writes) *)
  let buf = Sedlexing.Utf8.from_string "abcdef" in
  (match%sedlex buf with
    | ("abc" as _x), "def" | "a", ("bcd" as _x), "ey" ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=1 |}]

let%expect_test "as_bindings_multi_rule_mem_cells" =
  (* All rules in a match%sedlex share one pool of memory cells.
     The total is the sum of tags across ALL rules, not just the matched one. *)

  (* One rule with binding in tuple, one without: 0 cells (offsets known) *)
  let buf = Sedlexing.Utf8.from_string "ab" in
  (match%sedlex buf with
    | 'a', ('b' as _x) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | "cd" -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Even when the no-binding rule matches, no cells needed (offsets known) *)
  let buf = Sedlexing.Utf8.from_string "cd" in
  (match%sedlex buf with
    | 'a', ('b' as _x) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | "cd" -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Two rules, each with one binding: 0 cells (all offsets known) *)
  let buf = Sedlexing.Utf8.from_string "ab" in
  (match%sedlex buf with
    | 'a', ('b' as _x) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | 'c', ('d' as _y) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Three rules with one binding each: 0 cells (all offsets known) *)
  let buf = Sedlexing.Utf8.from_string "ab" in
  (match%sedlex buf with
    | 'a', ('b' as _x) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | 'c', ('d' as _y) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | 'e', ('f' as _z) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Whole-match + tuple binding: 0 cells (all offsets known) *)
  let buf = Sedlexing.Utf8.from_string "hello" in
  (match%sedlex buf with
    | Plus 'a' .. 'z' as _x -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | '0', (number as _y) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Or-pattern (disc elided) + tuple binding (offsets known): 0 cells *)
  let buf = Sedlexing.Utf8.from_string "123" in
  (match%sedlex buf with
    | (number as _x) | (Plus letter as _x) ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | '{', (Star (Compl '}') as _y), '}' ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}];
  (* Two or-pattern rules (both disc elided): 0 cells *)
  let buf = Sedlexing.Utf8.from_string "123" in
  (match%sedlex buf with
    | (number as _x) | (Plus letter as _x) ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | ('<' as _y) | ('>' as _y) -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}]

let%expect_test "as_bindings_nested_sedlex" =
  (* Regression: a nested match%sedlex in a case RHS must not reset the
     outer match's tag counter, which would cause init_mem/set_mem to be
     dropped and as-bindings to read uninitialized memory cells. *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | 'a', ('b' as x), 'c' ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | Star any -> (
        (* Nested match%sedlex in a case RHS *)
        Sedlexing.rollback buf;
        match%sedlex buf with
          | Plus 'a' .. 'z' -> Printf.printf "word\n"
          | _ -> Printf.printf "other\n")
    | _ -> assert false);
  [%expect {| x=b |}];
  (* Same but the nested match comes in a case BEFORE the as-binding rule *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | '0' .. '9' -> (
        Sedlexing.rollback buf;
        match%sedlex buf with
          | '0' .. '9' -> Printf.printf "digit\n"
          | _ -> Printf.printf "other\n")
    | Plus 'a' .. 'z' as x ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=abc |}];
  (* Verify the outer match still allocates memory cells *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | '0' .. '9' -> (
        Sedlexing.rollback buf;
        match%sedlex buf with
          | '0' .. '9' -> Printf.printf "digit\n"
          | _ -> Printf.printf "other\n")
    | Plus 'a' .. 'z' as _x -> Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=0 |}]

(* ------------------------------------------------------------------------ *)
(* Regression tests pinned to the current behavior. A test marked KNOWN BUG
   records what the generated code does today; the comment above each case
   states the expected result, and the fix flips the expect block. *)

(* KNOWN BUG (#199, repetition loop before a capture): the epsilon closure
   re-fires the capture's start tag on every iteration of a preceding Star, so
   the recorded start drifts to the last iteration. *)
let%expect_test "loop_before_capture" =
  let sub = Sedlexing.Latin1.of_submatch in
  (* expected x="abb" *)
  let buf = Sedlexing.Latin1.from_string "aabb" in
  (match%sedlex buf with
    | Star 'a', (('a', Plus 'b') as x) -> Printf.printf "x=%S\n" (sub x)
    | _ -> print_endline "nomatch");
  [%expect {| x="abb" |}];
  (* expected x="a" *)
  let buf = Sedlexing.Latin1.from_string "aab" in
  (match%sedlex buf with
    | Star 'a', (Plus 'a' as x), 'b' -> Printf.printf "x=%S\n" (sub x)
    | _ -> print_endline "nomatch");
  [%expect {| x="a" |}];
  (* the next two come out right today and pin the edge of the bug *)
  let buf = Sedlexing.Latin1.from_string "aba" in
  (match%sedlex buf with
    | Star 'a', (('a', 'a' .. 'c' | Star 'a' .. 'c') as y) ->
        Printf.printf "y=%S\n" (sub y)
    | _ -> print_endline "nomatch");
  [%expect {| y="ba" |}];
  let buf = Sedlexing.Latin1.from_string "bb" in
  (match%sedlex buf with
    | Star 'a', ('b' as y), Star 'b' -> Printf.printf "y=%S\n" (sub y)
    | _ -> print_endline "nomatch");
  [%expect {| y="b" |}]

(* [eof] is zero-width at runtime — [next] reports it without advancing — so
   the static offset optimization for captures must count it as zero code
   points, and a cset mixing [eof] with a real character has no fixed width at
   all. *)
let%expect_test "capture_before_eof" =
  let sub x =
    try Printf.sprintf "%S" (Sedlexing.Latin1.of_submatch x)
    with e -> "raises " ^ Printexc.to_string e
  in
  (* expected x="abc" *)
  let buf = Sedlexing.Latin1.from_string "abc" in
  (match%sedlex buf with
    | (Star any as x), eof -> Printf.printf "x=%s\n" (sub x)
    | _ -> print_endline "nomatch");
  [%expect {| x="abc" |}];
  (* expected x="a" *)
  let buf = Sedlexing.Latin1.from_string "a" in
  (match%sedlex buf with
    | ('a' as x), eof -> Printf.printf "x=%s\n" (sub x)
    | _ -> print_endline "nomatch");
  [%expect {| x="a" |}];
  (* expected x="" *)
  let buf = Sedlexing.Latin1.from_string "b" in
  (match%sedlex buf with
    | Star any, (Opt 'a' as x), eof -> Printf.printf "x=%s\n" (sub x)
    | _ -> print_endline "nomatch");
  [%expect {| x="" |}];
  (* expected x="x" on both inputs: the mixed cset is 0 or 1 wide *)
  let lex buf =
    match%sedlex buf with
      | ('x' as x), ('a' | eof) -> Printf.printf "x=%s\n" (sub x)
      | _ -> print_endline "nomatch"
  in
  lex (Sedlexing.Latin1.from_string "x");
  [%expect {| x="x" |}];
  lex (Sedlexing.Latin1.from_string "xa");
  [%expect {| x="x" |}]

(* KNOWN BUG (eof and rule priority): an earlier rule matching [s] must beat a
   later rule matching [s, eof] — same lexeme length, so declaration order
   breaks the tie — but eof's zero-width accept is reached last and overrides
   the earlier mark. *)
let%expect_test "eof_rule_priority" =
  let inputs = ["cc"; "c"; ""; "a"] in
  let run lex =
    List.iter
      (fun s ->
        Printf.printf "%-4S -> %s\n" s (lex (Sedlexing.Utf8.from_string s)))
      inputs
  in
  (* expected "cc" -> rule0 and "c" -> rule0 *)
  let lex buf =
    match%sedlex buf with
      | Plus ('b' | 'c') -> "rule0"
      | Star 'a' .. 'c', eof -> "rule1"
      | _ -> "none"
  in
  run lex;
  [%expect
    {|
    "cc" -> rule1
    "c"  -> rule1
    ""   -> rule1
    "a"  -> rule1
    |}];
  (* mirror: the eof-terminated rule is declared first and wins every tie *)
  let lex_eof_first buf =
    match%sedlex buf with
      | Star 'a' .. 'c', eof -> "rule0"
      | Plus ('b' | 'c') -> "rule1"
      | _ -> "none"
  in
  run lex_eof_first;
  [%expect
    {|
    "cc" -> rule0
    "c"  -> rule0
    ""   -> rule0
    "a"  -> rule0
    |}]

(* eof tie within a rule: the leftmost-greedy parse only becomes accepting
   through the zero-width [eof] arm, after the state reached by the last real
   character has already marked an equal-length parse of the same rule with a
   shorter capture. The eof arm's parse outranks it (its Star is still open in
   that state), so its mark must win. Today it does because [mark] keeps the
   last mark; a rule-priority fix for [eof_rule_priority] that breaks
   equal-length ties by "first mark wins" would regress this test. *)
let%expect_test "eof_zero_width_tie_within_rule" =
  let sub = Sedlexing.Latin1.of_submatch in
  (* expected x="aaa" and x="a" *)
  let lex buf =
    match%sedlex buf with
      | (Star 'a' as x), ('a' | eof) -> Printf.printf "x=%S\n" (sub x)
      | _ -> print_endline "nomatch"
  in
  lex (Sedlexing.Latin1.from_string "aaa");
  [%expect {| x="aaa" |}];
  lex (Sedlexing.Latin1.from_string "a");
  [%expect {| x="a" |}];
  (* expected x="b" and x="cb" *)
  let lex buf =
    match%sedlex buf with
      | (Star 'b' .. 'd' as x), ('b' | eof) -> Printf.printf "x=%S\n" (sub x)
      | _ -> print_endline "nomatch"
  in
  lex (Sedlexing.Latin1.from_string "b");
  [%expect {| x="b" |}];
  lex (Sedlexing.Latin1.from_string "cb");
  [%expect {| x="cb" |}];
  (* expected x="a": the left branch of the or-pattern parses "ab" too *)
  let buf = Sedlexing.Latin1.from_string "ab" in
  (match%sedlex buf with
    | ('a' as x), 'b', eof | 'a', ('b' as x) -> Printf.printf "x=%S\n" (sub x)
    | _ -> print_endline "nomatch");
  [%expect {| x="a" |}]

(* KNOWN BUG (eof self-loop): the test below is disabled until the bug is
   fixed. Its timeout relies on threads and Unix, which do not work on
   Windows in this setup. Uncomment it once the runtime no longer spins.

(* KNOWN BUG (eof self-loop): [eof] is reported without advancing, so an
   accepting state with an [eof] transition back to itself spins forever at end
   of input. Expected: every case matches rule0 and terminates.

   A spinning case cannot be interrupted, so each case runs on its own thread
   and the test moves on once a deadline passes; a runaway thread keeps
   spinning until the process exits. The terminating case therefore runs
   first, before any runaway thread competes for the scheduler. *)
let%expect_test "eof_self_loop_terminates" =
  let with_timeout f =
    let result = Atomic.make None in
    ignore
      (Thread.create
         (fun () ->
           let r = try f () with e -> Printexc.to_string e in
           Atomic.set result (Some r))
         ());
    let deadline = Unix.gettimeofday () +. 0.5 in
    while Atomic.get result = None && Unix.gettimeofday () < deadline do
      Thread.delay 0.01
    done;
    print_endline (Option.value (Atomic.get result) ~default:"TIMEOUT")
  in
  (* eof, eof matches today because the runtime reports eof repeatedly *)
  let buf = Sedlexing.Latin1.from_string "" in
  with_timeout (fun () ->
      match%sedlex buf with eof, eof -> "rule0" | _ -> "none");
  [%expect {| rule0 |}];
  let buf = Sedlexing.Latin1.from_string "" in
  with_timeout (fun () ->
      match%sedlex buf with Plus eof -> "rule0" | _ -> "none");
  [%expect {| TIMEOUT |}];
  let lex buf =
    match%sedlex buf with Star ('a' | eof) -> "rule0" | _ -> "none"
  in
  with_timeout (fun () -> lex (Sedlexing.Latin1.from_string "aa"));
  [%expect {| TIMEOUT |}];
  with_timeout (fun () -> lex (Sedlexing.Latin1.from_string ""));
  [%expect {| TIMEOUT |}];
  let buf = Sedlexing.Latin1.from_string "a" in
  with_timeout (fun () ->
      match%sedlex buf with Star 'a', Star eof -> "rule0" | _ -> "none");
  [%expect {| TIMEOUT |}]
*)

(* KNOWN BUG (#199, capture before a repetition loop): the mirror of
   [loop_before_capture]. The capture's end tag keeps firing inside the
   repetition that follows it, so the submatch swallows characters the rest of
   the rule then consumes, and the reported parse is one that cannot exist. *)
let%expect_test "capture_before_loop" =
  let sub x =
    try Printf.sprintf "%S" (Sedlexing.Latin1.of_submatch x)
    with e -> "raises " ^ Printexc.to_string e
  in
  let run inputs lex =
    List.iter
      (fun s ->
        Printf.printf "%-7S -> %s\n" s (lex (Sedlexing.Latin1.from_string s)))
      inputs
  in
  (* expected z="aa" and z="a": the Plus needs at least one 'a' *)
  run ["aaa"; "aa"] (fun buf ->
      match%sedlex buf with
        | (Rep ('a' .. 'c', 0 .. 2) as z), Plus 'a' -> "z=" ^ sub z
        | _ -> "nomatch");
  [%expect {|
    "aaa"   -> z="aa"
    "aa"    -> z="a"
    |}];
  (* expected z="a": the tail needs an even number of characters *)
  run ["aaaaa"] (fun buf ->
      match%sedlex buf with
        | (Rep ('a', 0 .. 2) as z), Plus ('a', 'a') -> "z=" ^ sub z
        | _ -> "nomatch");
  [%expect {| "aaaaa" -> z="a" |}];
  (* expected x="bd": one character is left for the middle class *)
  run ["bdc"] (fun buf ->
      match%sedlex buf with
        | (Plus 'b' .. 'd' as x), 'a' .. 'd', Star 'c' .. 'd' -> "x=" ^ sub x
        | _ -> "nomatch");
  [%expect {| "bdc"   -> x="bd" |}];
  (* expected x="b" y="b": y cannot be empty *)
  run ["bbac"] (fun buf ->
      match%sedlex buf with
        | (Rep ('b', 1 .. 3) as x), (Plus 'b' as y) ->
            "x=" ^ sub x ^ " y=" ^ sub y
        | _ -> "nomatch");
  [%expect {| "bbac"  -> x="b" y="b" |}];
  (* expected x="c" *)
  run ["ccd"] (fun buf ->
      match%sedlex buf with
        | (Star (Star 'c') as x), Plus 'a' .. 'c' -> "x=" ^ sub x
        | _ -> "nomatch");
  [%expect {| "ccd"   -> x="c" |}];
  (* expected x="a": the parse is ambiguous and the Star is greedy *)
  run ["aaa"] (fun buf ->
      match%sedlex buf with
        | Star 'a', (Plus 'a' as x) -> "x=" ^ sub x
        | _ -> "nomatch");
  [%expect {| "aaa"   -> x="a" |}];
  (* expected y="d" w="": today both submatches have garbage bounds and
     extracting them raises *)
  run ["ddd"] (fun buf ->
      match%sedlex buf with
        | Star 'd', ('a' .. 'd' as y), 'a' .. 'd', ((Star 'a' | 'b') as w) ->
            "y=" ^ sub y ^ " w=" ^ sub w
        | _ -> "nomatch");
  [%expect {| "ddd"   -> y="d" w="" |}];
  (* the fixed-width tail comes out right today and pins the edge of the bug *)
  run ["aa"; "aaa"] (fun buf ->
      match%sedlex buf with
        | (Rep ('a' .. 'c', 0 .. 2) as z), 'a' -> "z=" ^ sub z
        | _ -> "nomatch");
  [%expect {|
    "aa"    -> z="a"
    "aaa"   -> z="aa"
    |}]

(* Bounded repetition of a nullable body. [Rep (r, 0 .. 1)] unrolls to
   [r | ""], and [r] itself parses "" through its first alternative [Opt 'd']
   before it parses "b" through its second. Today the consuming parse wins in
   all four forms; a leftmost-first reading of the unrolling gives x="" for
   each, with the Star taking the 'b'. This test records the current answer so
   that a change of tie-break is visible. *)
let%expect_test "rep_nullable_body" =
  let sub = Sedlexing.Latin1.of_submatch in
  let run inputs lex =
    List.iter
      (fun s ->
        Printf.printf "%-4S -> %s\n" s (lex (Sedlexing.Latin1.from_string s)))
      inputs
  in
  run ["b"] (fun buf ->
      match%sedlex buf with
        | (Rep ((Opt 'd' | 'b'), 0 .. 1) as x), Star 'b' ->
            Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {| "b"  -> x="" |}];
  run ["b"] (fun buf ->
      match%sedlex buf with
        | ((Opt 'd' | 'b' | "") as x), Star 'b' -> Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {| "b"  -> x="" |}];
  run ["b"] (fun buf ->
      match%sedlex buf with
        | (Rep ((Opt 'd' | 'b'), 1 .. 2) as x), Star 'b' ->
            Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {| "b"  -> x="" |}];
  run ["bb"] (fun buf ->
      match%sedlex buf with
        | (Rep ((Opt 'd' | 'b'), 0 .. 2) as x), Star 'b' ->
            Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {| "bb" -> x="" |}]

(* Rep (_, 1 .. 1) is a single-character regexp, so Compl/Sub/Intersect
   accept it: it normalizes to the bare Chars node. *)
let%expect_test "rep_1_1_char_ops" =
  let buf = Sedlexing.Utf8.from_string "b" in
  (match%sedlex buf with
    | Compl (Rep ('a', 1 .. 1)) -> print_string "compl-ok\n"
    | _ -> assert false);
  [%expect {| compl-ok |}];
  let buf = Sedlexing.Utf8.from_string "b" in
  (match%sedlex buf with
    | Sub (any, Rep ('a', 1 .. 1)) -> print_string "sub-ok\n"
    | _ -> assert false);
  [%expect {| sub-ok |}];
  let buf = Sedlexing.Utf8.from_string "a" in
  (match%sedlex buf with
    | Intersect ('a' .. 'c', Rep ('a', 1 .. 1)) -> print_string "inter-ok\n"
    | _ -> assert false);
  [%expect {| inter-ok |}]

(* A nullable-only rule makes DFA state 0 an accepting sink; the generated
   code must not emit an empty `let rec` or call an undefined state function.
   [""] matches the zero-length prefix at position 0 regardless of input. *)
let%expect_test "empty_pattern" =
  let lex buf = match%sedlex buf with "" -> "empty" | _ -> "other" in
  Printf.printf "%s\n" (lex (Sedlexing.Utf8.from_string ""));
  Printf.printf "%s\n" (lex (Sedlexing.Utf8.from_string "abc"));
  [%expect {|
    empty
    empty
    |}]

(* Greedy repetition guards. These pass today; they pin the disambiguation
   rules that a rewrite of the determinization must preserve. *)

(* Opt is greedy (consume-first) and agrees with Rep (_, 0 .. 1): when the
   empty and the consuming parse have the same total length, the leading
   optional takes the character and the trailing Star stays empty. *)
let%expect_test "opt_greedy" =
  let buf = Sedlexing.Utf8.from_string "a" in
  (match%sedlex buf with
    | Opt 'a', (Star 'a' as x) ->
        Printf.printf "opt x=%S\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| opt x="a" |}];
  let buf = Sedlexing.Utf8.from_string "a" in
  (match%sedlex buf with
    | Rep ('a', 0 .. 1), (Star 'a' as x) ->
        Printf.printf "rep x=%S\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| rep x="" |}]

(* [Plus r] is [r, Star r]: after a first iteration that consumed nothing, a
   second (consuming) iteration still has priority over leaving the loop,
   exactly as for [Star]. The capture must be greedy in all three forms. *)
let%expect_test "plus_nullable_first_alternative" =
  let sub = Sedlexing.Latin1.of_submatch in
  let inputs = ["b"; "bb"; "db"] in
  let run lex =
    List.iter
      (fun s ->
        Printf.printf "%-4S -> %s\n" s (lex (Sedlexing.Latin1.from_string s)))
      inputs
  in
  run (fun buf ->
      match%sedlex buf with
        | (Plus (Opt 'd' | 'b') as x), Star 'b' -> Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {|
    "b"  -> x=""
    "bb" -> x=""
    "db" -> x="db"
    |}];
  run (fun buf ->
      match%sedlex buf with
        | (Star (Opt 'd' | 'b') as x), Star 'b' -> Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {|
    "b"  -> x="b"
    "bb" -> x="bb"
    "db" -> x="db"
    |}];
  run (fun buf ->
      match%sedlex buf with
        | (((Opt 'd' | 'b'), Star (Opt 'd' | 'b')) as x), Star 'b' ->
            Printf.sprintf "x=%S" (sub x)
        | _ -> "nomatch");
  [%expect {|
    "b"  -> x="b"
    "bb" -> x="bb"
    "db" -> x="db"
    |}]
