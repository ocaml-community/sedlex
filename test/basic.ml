let () = set_binary_mode_out stdout true
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

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
  (* Or-pattern with different offsets: 1 cell (disc only, positions known) *)
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
