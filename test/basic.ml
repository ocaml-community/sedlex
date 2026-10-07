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
  (* Test 10b: 3-way or-pattern reuses a single disc cell; the first and
     third branches bind x at the same offsets and share a disc value *)
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
  (* Test 11: delayed tag with backtracking (Opt at end) *)
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
     discriminator needs a cell. Its write is delayed until the branch
     reaches its final node, where the final operations set the canonical
     cell directly: no working register *)
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
  [%expect {| mem_cells=0 |}];
  (* Two rules that each need a tag: a token matches only one of them, so
     the two tags share one cell *)
  let lex buf =
    match%sedlex buf with
      | Plus 'a', (Plus 'b' as _x) ->
          Printf.printf "mem_cells=%d\n" (num_mem buf)
      | Plus 'c', (Plus 'd' as _y) ->
          Printf.printf "mem_cells=%d\n" (num_mem buf)
      | _ -> assert false
  in
  lex (Sedlexing.Utf8.from_string "ab");
  [%expect {| mem_cells=1 |}];
  lex (Sedlexing.Utf8.from_string "cd");
  [%expect {| mem_cells=1 |}]

let%expect_test "as_bindings_nested_sedlex" =
  (* Regression: a nested match%sedlex in a case RHS must not reset the
     outer match's tag counter, which would cause ensure_mem/set_mem to be
     dropped and as-bindings to read uninitialized memory cells. The outer
     bindings need a tag (variable-length prefix). *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | Plus 'a', ('b' as x), Plus 'c' ->
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
    | Plus 'a', (Plus 'b' .. 'z' as x) ->
        Printf.printf "x=%s\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| x=bc |}];
  (* Verify the outer match still allocates memory cells *)
  let buf = Sedlexing.Utf8.from_string "abc" in
  (match%sedlex buf with
    | '0' .. '9' -> (
        Sedlexing.rollback buf;
        match%sedlex buf with
          | '0' .. '9' -> Printf.printf "digit\n"
          | _ -> Printf.printf "other\n")
    | Plus 'a', (Plus 'b' .. 'z' as _x) ->
        Printf.printf "mem_cells=%d\n" (num_mem buf)
    | _ -> assert false);
  [%expect {| mem_cells=1 |}]

(* ------------------------------------------------------------------------ *)
(* Regression tests. They were first pinned to the behavior of the time and
   flipped by the fixes; the comment above each case says what used to go
   wrong. *)

(* Regression (#199, repetition loop before a capture): the epsilon closure
   used to re-fire the capture's start tag on every iteration of a preceding
   Star, so the recorded start drifted to the last iteration. *)
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
  (* the next two were never affected and pin the edge of the bug *)
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

(* End of input counts as one more symbol read: a rule matching [s, eof]
   beats an earlier rule matching [s]. 3.8 let declaration order decide
   instead, and a lexer with a nullable rule before [eof] looped forever. *)
let%expect_test "eof_longest_match" =
  let inputs = ["cc"; "c"; ""; "a"] in
  let run lex =
    List.iter
      (fun s ->
        Printf.printf "%-4S -> %s\n" s (lex (Sedlexing.Utf8.from_string s)))
      inputs
  in
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
  (* mirror: the eof-terminated rule is declared first *)
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
    |}];
  (* two rules reading eof: declaration order decides *)
  let lex_both buf =
    match%sedlex buf with
      | Plus ('b' | 'c'), eof -> "rule0"
      | Star 'a' .. 'c', eof -> "rule1"
      | _ -> "none"
  in
  run lex_both;
  [%expect
    {|
    "cc" -> rule0
    "c"  -> rule0
    ""   -> rule1
    "a"  -> rule1
    |}];
  (* a nullable rule before [eof], the usual shape of a lexer loop *)
  let rec tokens fuel acc buf =
    if fuel = 0 then "loops"
    else (
      match%sedlex buf with
        | '@' -> tokens (fuel - 1) ("@" :: acc) buf
        | Star (Sub (any, '@')) ->
            tokens (fuel - 1) (Sedlexing.Utf8.lexeme buf :: acc) buf
        | eof -> String.concat " " (List.rev_map (Printf.sprintf "%S") acc)
        | _ -> assert false)
  in
  List.iter
    (fun s ->
      Printf.printf "%-8S -> %s\n" s
        (tokens 100 [] (Sedlexing.Utf8.from_string s)))
    ["ab@cd"; "@@"; ""];
  [%expect
    {|
    "ab@cd"  -> "ab" "@" "cd"
    "@@"     -> "@" "@"
    ""       ->
    |}];
  (* the final operations of the rule accepting before eof are overridden *)
  let sub = Sedlexing.Latin1.of_submatch in
  let lex buf =
    match%sedlex buf with
      | Plus 'a' as x -> Printf.printf "rule0 x=%S\n" (sub x)
      | (Star 'a' as x), 'a', eof -> Printf.printf "rule1 x=%S\n" (sub x)
      | _ -> print_endline "nomatch"
  in
  lex (Sedlexing.Latin1.from_string "aaa");
  [%expect {| rule1 x="aa" |}];
  lex (Sedlexing.Latin1.from_string "aab");
  [%expect {| rule0 x="aa" |}]

(* eof within a rule: the leftmost-greedy parse only becomes accepting
   through the [eof] arm, after the state reached by the last real character
   has already marked a parse of the same rule with a shorter capture. The
   parse reading eof is the longer one: its transition returns directly over
   the mark. *)
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
  [%expect {| x="a" |}];
  (* mirror: the right branch reads eof and beats the leftmost one *)
  let lex buf =
    match%sedlex buf with
      | 'a', ('b' as x) | ('a' as x), 'b', eof -> Printf.printf "x=%S\n" (sub x)
      | _ -> print_endline "nomatch"
  in
  lex (Sedlexing.Latin1.from_string "ab");
  [%expect {| x="a" |}];
  lex (Sedlexing.Latin1.from_string "abc");
  [%expect {| x="b" |}]

(* Regression (eof self-loop): end of input is read once. [eof] used to be
   reported again and again without advancing, so an accepting state with an
   [eof] transition back to itself spun forever at end of input, and
   [eof, eof] matched. *)
let%expect_test "eof_self_loop_terminates" =
  let run lex inputs =
    List.iter
      (fun s ->
        let buf = Sedlexing.Latin1.from_string s in
        let r = lex buf in
        Printf.printf "%-4S -> %S %s\n" s (Sedlexing.Latin1.lexeme buf) r)
      inputs
  in
  run
    (fun buf -> match%sedlex buf with Plus eof -> "rule0" | _ -> "none")
    [""; "a"];
  [%expect {|
    ""   -> "" rule0
    "a"  -> "" none
    |}];
  run
    (fun buf ->
      match%sedlex buf with Star ('a' | eof) -> "rule0" | _ -> "none")
    ["aa"; ""; "ab"];
  [%expect
    {|
    "aa" -> "aa" rule0
    ""   -> "" rule0
    "ab" -> "a" rule0
    |}];
  run
    (fun buf ->
      match%sedlex buf with Star 'a', Star eof -> "rule0" | _ -> "none")
    ["a"; ""];
  [%expect {|
    "a"  -> "a" rule0
    ""   -> "" rule0
    |}];
  (* the second eof has nothing left to read *)
  run
    (fun buf -> match%sedlex buf with eof, eof -> "rule0" | _ -> "none")
    [""];
  [%expect {| ""   -> "" none |}]

(* Regression (#199, capture before a repetition loop): the mirror of
   [loop_before_capture]. The capture's end tag used to keep firing inside the
   repetition that follows it, so the submatch swallowed characters the rest
   of the rule then consumed, and the reported parse was one that cannot
   exist. *)
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
  (* expected y="d" w="": both submatches used to have garbage bounds, and
     extracting them raised *)
  run ["ddd"] (fun buf ->
      match%sedlex buf with
        | Star 'd', ('a' .. 'd' as y), 'a' .. 'd', ((Star 'a' | 'b') as w) ->
            "y=" ^ sub y ^ " w=" ^ sub w
        | _ -> "nomatch");
  [%expect {| "ddd"   -> y="d" w="" |}];
  (* the fixed-width tail was never affected and pins the edge of the bug *)
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
   before it parses "b" through its second. Leftmost-greedy takes that first
   parse in all four forms: x="", with the Star taking the 'b'. The consuming
   parse used to win; this test makes a change of tie-break visible. *)
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

(* Falling back after a longer match failed must give the sub-matches of
   the last accepting state, though [Sedlexing.backtrack] restores no cell.
   Each pattern accepts, goes on with a longer alternative that records the
   same sub-match again, then fails (first input) or succeeds (second). *)
let%expect_test "fallback_keeps_submatches" =
  let sub x = Printf.sprintf "%S" (Sedlexing.Latin1.of_submatch x) in
  let run inputs lex =
    List.iter
      (fun s ->
        let buf = Sedlexing.Latin1.from_string s in
        let r = lex buf in
        Printf.printf "%-9S -> %S %s\n" s (Sedlexing.Latin1.lexeme buf) r)
      inputs
  in
  run ["azdcwX"; "azdcwd"; "adX"] (fun buf ->
      match%sedlex buf with
        | ("a" | "azdc"), (Star ('z' | 'w') as x), 'd' -> "x=" ^ sub x
        | _ -> "nomatch");
  [%expect
    {|
    "azdcwX"  -> "azd" x="z"
    "azdcwd"  -> "azdcwd" x="w"
    "adX"     -> "ad" x=""
    |}];
  (* same with an or-pattern: the longer branch must not leak into the
     discriminator either *)
  run ["azdcwX"; "azdcwwe"] (fun buf ->
      match%sedlex buf with
        | "a", (Star 'z' as x), 'd' | "azdc", (Plus 'w' as x), 'e' ->
            "x=" ^ sub x
        | _ -> "nomatch");
  [%expect
    {|
    "azdcwX"  -> "azd" x="z"
    "azdcwwe" -> "azdcwwe" x="ww"
    |}];
  (* two sub-matches, only the second one recorded again *)
  run ["abzcbwX"; "abzcbwc"] (fun buf ->
      match%sedlex buf with
        | (Plus 'a' as x), ("b" | "bzcb"), (Star ('z' | 'w') as y), 'c' ->
            "x=" ^ sub x ^ " y=" ^ sub y
        | _ -> "nomatch");
  [%expect
    {|
    "abzcbwX" -> "abzc" x="a" y="z"
    "abzcbwc" -> "abzcbwc" x="a" y="w"
    |}]

(* Tags of different rules share memory cells when their lifetimes allow.
   Falling back from rule 1 to rule 0 must still give rule 0's sub-match. *)
let%expect_test "submatches_with_shared_cells" =
  let sub x = Printf.sprintf "%S" (Sedlexing.Latin1.of_submatch x) in
  List.iter
    (fun s ->
      let buf = Sedlexing.Latin1.from_string s in
      let r =
        match%sedlex buf with
          | (Plus 'a' as x), Plus 'b' -> "rule 0 x=" ^ sub x
          | Plus 'a', Plus 'b', (Plus 'c' as y), 'd' -> "rule 1 y=" ^ sub y
          | (Plus 'e' as z), 'f' -> "rule 2 z=" ^ sub z
          | _ -> "nomatch"
      in
      Printf.printf "%-9S -> %S %s\n" s (Sedlexing.Latin1.lexeme buf) r)
    ["aabb"; "aabbccd"; "aabbccX"; "eef"];
  [%expect
    {|
    "aabb"    -> "aabb" rule 0 x="aa"
    "aabbccd" -> "aabbccd" rule 1 y="cc"
    "aabbccX" -> "aabb" rule 0 x="aa"
    "eef"     -> "eef" rule 2 z="ee"
    |}]

(* A path still in x reaches the final node with the end of x not recorded
   yet, while a path already in y holds its own value for it. *)
let%expect_test "submatch_set_on_accept_while_held" =
  let sub x = Printf.sprintf "%S" (Sedlexing.Latin1.of_submatch x) in
  List.iter
    (fun s ->
      let buf = Sedlexing.Latin1.from_string s in
      match%sedlex buf with
        | (Star 'a' .. 'c' as x), (Star ('a' .. 'b', 'c' .. 'd') as y) ->
            Printf.printf "%-8S -> x=%s y=%s\n" s (sub x) (sub y)
        | _ -> print_endline "nomatch")
    ["cbdbc"; "cbd"; "acbc"; "bc"];
  [%expect
    {|
    "cbdbc"  -> x="c" y="bdbc"
    "cbd"    -> x="c" y="bd"
    "acbc"   -> x="acbc" y=""
    "bc"     -> x="bc" y=""
    |}]

(* Transition operations record the position before the character read, but
   reading end of input does not advance: sub-matches that end where eof, or
   either eof or a character, is read must get their end right. *)
let%expect_test "submatch_end_before_eof_or_char" =
  let sub x = Printf.sprintf "%S" (Sedlexing.Latin1.of_submatch x) in
  let run inputs lex =
    List.iter
      (fun s ->
        let buf = Sedlexing.Latin1.from_string s in
        let r = lex buf in
        Printf.printf "%-6S -> %S %s\n" s (Sedlexing.Latin1.lexeme buf) r)
      inputs
  in
  (* eof or 'b' *)
  run ["aa"; "aab"; "a"; "ab"] (fun buf ->
      match%sedlex buf with
        | (Plus 'a' as x), (eof | 'b') -> "x=" ^ sub x
        | _ -> "nomatch");
  [%expect
    {|
    "aa"   -> "aa" x="aa"
    "aab"  -> "aab" x="aa"
    "a"    -> "a" x="a"
    "ab"   -> "ab" x="a"
    |}];
  (* eof after an optional tail *)
  run ["aa"; "aac"; "aacc"] (fun buf ->
      match%sedlex buf with
        | (Plus 'a' as x), Star 'c', eof -> "x=" ^ sub x
        | _ -> "nomatch");
  [%expect
    {|
    "aa"   -> "aa" x="aa"
    "aac"  -> "aac" x="aa"
    "aacc" -> "aacc" x="aa"
    |}]

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

(* Greedy repetition guards: they pin the disambiguation rules that the
   determinization must preserve. *)

(* Opt is greedy (consume-first) and agrees with Rep (_, 0 .. 1): when the
   empty and the consuming parse have the same total length, the leading
   optional takes the character and the trailing Star stays empty. *)
let%expect_test "opt_greedy" =
  let buf = Sedlexing.Utf8.from_string "a" in
  (match%sedlex buf with
    | Opt 'a', (Star 'a' as x) ->
        Printf.printf "opt x=%S\n" (Sedlexing.Utf8.of_submatch x)
    | _ -> assert false);
  [%expect {| opt x="" |}];
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
    "b"  -> x="b"
    "bb" -> x="bb"
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

let%expect_test "latin1_sub_lexeme" =
  let buf = Sedlexing.Latin1.from_string "h\233llo wor\255d" in
  let sub pos len =
    match Sedlexing.Latin1.sub_lexeme buf pos len with
      | s -> Printf.printf "%d %d -> %S\n" pos len s
      | exception Invalid_argument _ ->
          Printf.printf "%d %d -> Invalid_argument\n" pos len
  in
  (match%sedlex buf with
    | Plus (Compl ' ') ->
        Printf.printf "%S\n" (Sedlexing.Latin1.lexeme buf);
        sub 0 5;
        sub 1 3;
        sub 5 0;
        sub 2 (-1);
        sub (-1) 2;
        (* past the lexeme, inside the buffer *)
        sub 0 6;
        sub 4 2;
        sub 0 100
    | _ -> assert false);
  [%expect
    {|
    "h\233llo"
    0 5 -> "h\233llo"
    1 3 -> "\233ll"
    5 0 -> ""
    2 -1 -> Invalid_argument
    -1 2 -> Invalid_argument
    0 6 -> Invalid_argument
    4 2 -> Invalid_argument
    0 100 -> Invalid_argument
    |}];
  (* A code point outside Latin1 *)
  let buf = Sedlexing.Utf8.from_string "a\xe2\x82\xacb" in
  (match%sedlex buf with
    | Plus any -> (
        match Sedlexing.Latin1.lexeme buf with
          | s -> Printf.printf "%S\n" s
          | exception Sedlexing.InvalidCodepoint c -> Printf.printf "U+%04X\n" c
        )
    | _ -> assert false);
  [%expect {| U+20AC |}]

(* [Utf8.sub_lexeme] and [Utf16.sub_lexeme] against the encoders of the
   standard library, on every sub-range of a lexeme that mixes all the
   encoding lengths. *)
let%expect_test "utf_sub_lexeme_encoding" =
  let cps =
    [|
      0x41;
      0x00;
      0x7a;
      0x7F;
      0x80;
      0x7FF;
      0x800;
      0xD7FF;
      0xE000;
      0xFEFF;
      0xFFFD;
      0xFFFF;
      0x10000;
      0x1F600;
      0x10FFFF;
      0x62;
      0x63;
    |]
  in
  let a = Array.map Uchar.of_int cps in
  let n = Array.length a in
  let buf = Sedlexing.from_uchar_array a in
  (match%sedlex buf with Plus any -> () | _ -> assert false);
  assert (Sedlexing.lexeme_length buf = n);
  let reference add ~bom pos len =
    let b = Buffer.create 16 in
    if bom then add b (Uchar.of_int 0xFEFF);
    for i = pos to pos + len - 1 do
      add b a.(i)
    done;
    Buffer.contents b
  in
  let errors = ref 0 and checked = ref 0 in
  let check what got expected =
    incr checked;
    if got <> expected then begin
      incr errors;
      Printf.printf "%s: got %S, expected %S\n" what got expected
    end
  in
  for pos = 0 to n do
    for len = 0 to n - pos do
      check
        (Printf.sprintf "utf8 %d %d" pos len)
        (Sedlexing.Utf8.sub_lexeme buf pos len)
        (reference Buffer.add_utf_8_uchar ~bom:false pos len);
      List.iter
        (fun (name, bo, add) ->
          List.iter
            (fun bom ->
              check
                (Printf.sprintf "utf16%s %d %d bom=%b" name pos len bom)
                (Sedlexing.Utf16.sub_lexeme buf pos len bo bom)
                (reference add ~bom pos len))
            [false; true])
        [
          ("le", Sedlexing.Utf16.Little_endian, Buffer.add_utf_16le_uchar);
          ("be", Sedlexing.Utf16.Big_endian, Buffer.add_utf_16be_uchar);
        ]
    done
  done;
  check "utf8 lexeme"
    (Sedlexing.Utf8.lexeme buf)
    (reference Buffer.add_utf_8_uchar ~bom:false 0 n);
  check "utf16 lexeme"
    (Sedlexing.Utf16.lexeme buf Sedlexing.Utf16.Big_endian true)
    (reference Buffer.add_utf_16be_uchar ~bom:true 0 n);
  Printf.printf "%d checks, %d errors\n" !checked !errors;
  [%expect {| 857 checks, 0 errors |}]

let%expect_test "utf_sub_lexeme_range" =
  let buf = Sedlexing.Utf8.from_string "h\xc3\xa9llo w\xc3\xb6rld" in
  let show name f pos len =
    match f pos len with
      | s -> Printf.printf "%s %d %d -> %S\n" name pos len s
      | exception Invalid_argument _ ->
          Printf.printf "%s %d %d -> Invalid_argument\n" name pos len
  in
  let uchars =
    show "uchars" (fun pos len ->
        Sedlexing.sub_lexeme buf pos len
        |> Array.map (fun c -> Printf.sprintf "%X" (Uchar.to_int c))
        |> Array.to_list |> String.concat " ")
  in
  let utf8 = show "utf8" (Sedlexing.Utf8.sub_lexeme buf) in
  let utf16 =
    show "utf16" (fun pos len ->
        Sedlexing.Utf16.sub_lexeme buf pos len Sedlexing.Utf16.Big_endian true)
  in
  (match%sedlex buf with
    | Plus (Compl ' ') ->
        List.iter
          (fun f ->
            f 0 5;
            f 1 3;
            f 5 0;
            f 2 (-1);
            f (-1) 2;
            (* past the lexeme, inside the buffer *)
            f 0 6;
            f 4 2;
            f 0 100)
          [uchars; utf8; utf16]
    | _ -> assert false);
  [%expect
    {|
    uchars 0 5 -> "68 E9 6C 6C 6F"
    uchars 1 3 -> "E9 6C 6C"
    uchars 5 0 -> ""
    uchars 2 -1 -> Invalid_argument
    uchars -1 2 -> Invalid_argument
    uchars 0 6 -> Invalid_argument
    uchars 4 2 -> Invalid_argument
    uchars 0 100 -> Invalid_argument
    utf8 0 5 -> "h\195\169llo"
    utf8 1 3 -> "\195\169ll"
    utf8 5 0 -> ""
    utf8 2 -1 -> Invalid_argument
    utf8 -1 2 -> Invalid_argument
    utf8 0 6 -> Invalid_argument
    utf8 4 2 -> Invalid_argument
    utf8 0 100 -> Invalid_argument
    utf16 0 5 -> "\254\255\000h\000\233\000l\000l\000o"
    utf16 1 3 -> "\254\255\000\233\000l\000l"
    utf16 5 0 -> "\254\255"
    utf16 2 -1 -> Invalid_argument
    utf16 -1 2 -> Invalid_argument
    utf16 0 6 -> Invalid_argument
    utf16 4 2 -> Invalid_argument
    utf16 0 100 -> Invalid_argument
    |}]

let%expect_test "utf_of_submatch_non_ascii" =
  let buf =
    Sedlexing.Utf8.from_string
      "h\xc3\xa9llo w\xc3\xb6rld\xe2\x82\xac\xf0\x9f\x98\x80"
  in
  (match%sedlex buf with
    | (Plus (Compl ' ') as x), ' ', (Plus any as y) ->
        Printf.printf "utf8: %S %S\n"
          (Sedlexing.Utf8.of_submatch x)
          (Sedlexing.Utf8.of_submatch y);
        Printf.printf "utf16le: %S\n"
          (Sedlexing.Utf16.of_submatch y Sedlexing.Utf16.Little_endian false);
        Printf.printf "utf16be+bom: %S\n"
          (Sedlexing.Utf16.of_submatch x Sedlexing.Utf16.Big_endian true)
    | _ -> assert false);
  [%expect
    {|
    utf8: "h\195\169llo" "w\195\182rld\226\130\172\240\159\152\128"
    utf16le: "w\000\246\000r\000l\000d\000\172 =\216\000\222"
    utf16be+bom: "\254\255\000h\000\233\000l\000l\000o"
    |}]

(* Extraction from tokens that do not start the buffer, read through a
   generator so that the buffer is refilled and compacted several times. The
   reference is built from the code points of [Sedlexing.lexeme]. *)
let%expect_test "sub_lexeme_later_tokens" =
  let run name ~latin1 cps =
    let text =
      Array.init 3000 (fun i ->
          Uchar.of_int
            (if i mod 7 = 3 then 0x20 else cps.(i * 5 mod Array.length cps)))
    in
    let buf =
      let i = ref 0 in
      Sedlexing.from_gen (fun () ->
          if !i >= Array.length text then None
          else (
            incr i;
            Some text.(!i - 1)))
    in
    let encode add a =
      let b = Buffer.create 16 in
      Array.iter (add b) a;
      Buffer.contents b
    in
    let tokens = ref 0 and errors = ref 0 in
    let check what got expected =
      if got <> expected then begin
        incr errors;
        if !errors <= 5 then
          Printf.printf "%s, token %d, %s: got %S, expected %S\n" name !tokens
            what got expected
      end
    in
    let token () =
      incr tokens;
      let a = Sedlexing.lexeme buf in
      let n = Array.length a in
      (* the whole lexeme, then without its first and last code points *)
      List.iter
        (fun (pos, len) ->
          let a = Array.sub a pos len in
          check "utf8"
            (Sedlexing.Utf8.sub_lexeme buf pos len)
            (encode Buffer.add_utf_8_uchar a);
          check "utf16le"
            (Sedlexing.Utf16.sub_lexeme buf pos len
               Sedlexing.Utf16.Little_endian false)
            (encode Buffer.add_utf_16le_uchar a);
          check "utf16be"
            (Sedlexing.Utf16.sub_lexeme buf pos len Sedlexing.Utf16.Big_endian
               true)
            ("\xfe\xff" ^ encode Buffer.add_utf_16be_uchar a);
          if latin1 then
            check "latin1"
              (Sedlexing.Latin1.sub_lexeme buf pos len)
              (encode (fun b u -> Buffer.add_char b (Uchar.to_char u)) a))
        [(0, n); (1, n - 1); (0, n - 1)]
    in
    let rec loop () =
      match%sedlex buf with
        | Plus (Compl ' ') | ' ' ->
            token ();
            loop ()
        | eof -> ()
        | _ -> assert false
    in
    loop ();
    Printf.printf "%s: %d tokens, %d errors\n" name !tokens !errors
  in
  run "all widths" ~latin1:false
    [| 0x61; 0xE9; 0x20AC; 0x1F600; 0x62; 0x7F; 0x80; 0x7FF |];
  run "latin1" ~latin1:true [| 0x61; 0xE9; 0xFF; 0x62; 0x7F; 0x80 |];
  [%expect
    {|
    all widths: 858 tokens, 0 errors
    latin1: 858 tokens, 0 errors
    |}]

(* The memory cells are not cleared between tokens: each action must only
   see the cells written by its own match, whatever earlier tokens and other
   rules sharing the lexbuf left behind. *)
let%expect_test "stale_cells_between_tokens" =
  let sub = Sedlexing.Utf8.of_submatch in
  let rec other buf =
    match%sedlex buf with
      | (Plus 'x' as a), (Plus 'y' as b), (Plus 'z' as c), Plus 'x' ->
          Printf.printf "other: a=%S b=%S c=%S\n" (sub a) (sub b) (sub c);
          token buf
      | _ -> token buf
  and token buf =
    match%sedlex buf with
      | (Plus 'a' as k), '=', (Plus 'b' as v)
      | (Plus 'b' as v), ':', (Plus 'a' as k) ->
          Printf.printf "pair: k=%S v=%S\n" (sub k) (sub v);
          other buf
      | Plus 'a', (Plus 'c' as v) | (Plus 'c' as v), Plus 'b' ->
          Printf.printf "single: v=%S\n" (sub v);
          other buf
      | ' ' -> other buf
      | eof -> ()
      | _ -> assert false
  in
  token
    (Sedlexing.Utf8.from_string
       "aaa=b bb:a a=bbb xxyzzzx b:aa acc cb aaaac xyyzx ccccbb");
  [%expect
    {|
    pair: k="aaa" v="b"
    pair: k="a" v="bb"
    pair: k="a" v="bbb"
    other: a="xx" b="y" c="zzz"
    pair: k="aa" v="b"
    single: v="cc"
    single: v="c"
    single: v="c"
    other: a="x" b="yy" c="z"
    single: v="cccc"
    |}]

(* Same as above with the cells overwritten before every token, by the value
   they used to be cleared with (-1), by plausible positions and by encoded
   discriminator values: the submatches must not depend on it. The input is
   read through a generator and is long enough for the buffer to be refilled,
   which shifts the positions left in the cells. *)
let%expect_test "poisoned_cells" =
  let sub = Sedlexing.Utf8.of_submatch in
  let digit = [%sedlex.regexp? '0' .. '9'] in
  let rule_a buf =
    match%sedlex buf with
      (* nested or-patterns *)
      | ('a', (Plus 'x' as v) | 'b', (Plus 'y' as v)), ';'
      | 'c', ((Plus 'z' as v) | (Plus 'w' as v)), ';' ->
          Some ("nested " ^ sub v)
      (* a loop before an overlapping capture *)
      | Star 'a', (Plus ('a' | 'b') as u), '!' -> Some ("loop " ^ sub u)
      | (Plus digit as n), '.', (Plus digit as f) ->
          Some ("float " ^ sub n ^ " " ^ sub f)
      (* a capture that may end at the end of input *)
      | '#', (Star (Compl ' ') as c), (' ' | eof) -> Some ("comment " ^ sub c)
      | ' ' -> Some "space"
      | eof -> None
      | any -> Some ("any " ^ Sedlexing.Utf8.lexeme buf)
      | _ -> assert false
  in
  let rule_b buf =
    match%sedlex buf with
      | (Plus 'k' as k), '=', (Plus 'v' as v)
      | (Plus 'v' as v), ':', (Plus 'k' as k) ->
          Some ("pair " ^ sub k ^ " " ^ sub v)
      | (Plus digit as n), Opt ('e', Plus digit) -> Some ("int " ^ sub n)
      | ' ' -> Some "space"
      | eof -> None
      | any -> Some ("any " ^ Sedlexing.Utf8.lexeme buf)
      | _ -> assert false
  in
  let input =
    let words =
      [|
        "axx;";
        "kk=v";
        "byyy;";
        "v:k";
        "czz;";
        "12";
        "cw;";
        "vvv:kk";
        "aab!";
        "k=vv";
        "ab!";
        "7";
        "bb!";
        "kkk=vvv";
        "12.5";
        "v:kkk";
        "3.14159";
        "42";
        "#note";
        "k=v";
        "x";
        "y";
      |]
    in
    let b = Buffer.create 4096 in
    for i = 0 to 399 do
      Buffer.add_string b words.(i * 7 mod Array.length words);
      Buffer.add_char b ' '
    done;
    Buffer.add_string b "#end";
    Buffer.contents b
  in
  let run poison =
    let buf = Sedlexing.Utf8.from_gen (gen_from_string input) in
    Sedlexing.__private__ensure_mem buf 16;
    let out = Buffer.create 4096 in
    let seed = ref 42 in
    let rec loop i =
      (match poison with
        | None -> ()
        | Some v ->
            for c = 0 to Sedlexing.__private__num_mem_cells buf - 1 do
              Sedlexing.__private__mem_set buf c v
            done);
      (* two rules with different cells, picked by a fixed pseudo-random
         sequence so that every word meets both *)
      seed := ((!seed * 1103515245) + 12345) land 0x3FFFFFFF;
      match if (!seed lsr 16) land 1 = 0 then rule_a buf else rule_b buf with
        | None -> i
        | Some s ->
            Buffer.add_string out s;
            Buffer.add_char out '\n';
            loop (i + 1)
    in
    let n = loop 0 in
    (n, Buffer.contents out)
  in
  let n, reference = run None in
  Printf.printf "%d characters, %d tokens\n" (String.length input) n;
  (* how many tokens of each kind, and a few of them *)
  let lines = String.split_on_char '\n' reference in
  let kind l = List.hd (String.split_on_char ' ' l) in
  List.iter
    (fun k ->
      let ls = List.filter (fun l -> kind l = k) lines in
      Printf.printf "%-8s %4d  e.g. %s\n" k (List.length ls)
        (String.concat ", "
           (List.sort_uniq compare ls |> List.filteri (fun i _ -> i < 4))))
    ["nested"; "loop"; "float"; "comment"; "pair"; "int"];
  List.iter
    (fun v ->
      Printf.printf "cells set to %d: %s\n" v
        (if run (Some v) = (n, reference) then "same" else "DIFFERENT"))
    [-1; 0; 1; 3; 600; 1_000_003; -2; -3; -4; -5];
  [%expect
    {|
    1877 characters, 1162 tokens
    nested     39  e.g. nested w, nested xx, nested yyy, nested zz
    loop       43  e.g. loop b, loop bb
    float      24  e.g. float 12 5, float 3 14159
    comment    11  e.g. comment end, comment note
    pair       83  e.g. pair k v, pair k vv, pair k vvv, pair kk v
    int        61  e.g. int 12, int 14159, int 2, int 3
    cells set to -1: same
    cells set to 0: same
    cells set to 1: same
    cells set to 3: same
    cells set to 600: same
    cells set to 1000003: same
    cells set to -2: same
    cells set to -3: same
    cells set to -4: same
    cells set to -5: same
    |}]
