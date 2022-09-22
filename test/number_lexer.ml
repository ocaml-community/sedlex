let digit_2 = [%sedlex.regexp? '0' .. '1']
let digit_8 = [%sedlex.regexp? '0' .. '7']
let digit = [%sedlex.regexp? '0' .. '9']
let digit_16 = [%sedlex.regexp? digit | 'A' .. 'F' | 'a' .. 'f']
let prefix_2 = [%sedlex.regexp? "0b"]
let prefix_8 = [%sedlex.regexp? "0o"]
let prefix_16 = [%sedlex.regexp? "0x"]
let sign = [%sedlex.regexp? "" | '+' | '-']
let sign_op = [%sedlex.regexp? '+' | '-']
let num_2 = [%sedlex.regexp? Plus digit_2]
let num_8 = [%sedlex.regexp? Plus digit_8]
let num_10 = [%sedlex.regexp? Plus digit]
let num_16 = [%sedlex.regexp? Plus digit_16]

let rec token buf =
  let sub (a, b) = Sedlexing.Latin1.sub_lexeme buf a b in
  match%sedlex buf with
    (* {Integers} *)
    | (sign as s), prefix_2, (num_2 as n) ->
        Printf.printf "Bin %s%s\n" (sub s) (sub n);
        token buf
    | (sign as s), prefix_8, (num_8 as n) ->
        Printf.printf "Oct %s%s\n" (sub s) (sub n);
        token buf
    | (sign as s), (num_10 as n) ->
        Printf.printf "Dec %s%s\n" (sub s) (sub n);
        token buf
    | (sign as s), prefix_16, (num_16 as n) ->
        Printf.printf "Hex %s%s\n" (sub s) (sub n);
        token buf
    (* {Fractions} *)
    | (sign as s), prefix_2, (num_2 as n), '/', (num_2 as d) ->
        Printf.printf "Bin %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | (sign as s), prefix_8, (num_8 as n), '/', (num_8 as d) ->
        Printf.printf "Oct %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | (sign as s), (num_10 as n), '/', (num_10 as d) ->
        Printf.printf "Dec %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | (sign as s), prefix_16, (num_16 as n), '/', (num_16 as d) ->
        Printf.printf "Hex %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    (* {Complex Numbers} *)
    | (sign as s), prefix_2, (num_2 as r), (sign_op as o), (num_2 as i), 'i' ->
        Printf.printf "Bin %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | (sign as s), prefix_8, (num_8 as r), (sign_op as o), (num_8 as i), 'i' ->
        Printf.printf "Oct %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | (sign as s), (num_10 as r), (sign_op as o), (num_10 as i), 'i' ->
        Printf.printf "Dec %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | (sign as s), prefix_16, (num_16 as r), (sign_op as o), (num_16 as i), 'i'
      ->
        Printf.printf "Hex %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    (* {Others} *)
    | Plus xml_blank -> token buf
    | 128 .. 255 -> print_endline "Non ASCII"
    | eof -> print_endline "EOF"
    | _ -> failwith "Unexpected character"

let%expect_test _ =
  let lexbuf =
    Sedlexing.Latin1.from_string
      {|
        123
        +123
        -123
        0b01010101
        -0b11110000
        +0b11111111
        0o12345670
        +0o76543210
        -0o17263540
        0x123abcdef
        -0x456DEFabc
        +0x789ABCdef
        123/456
        -456/789
        +987/654
        0o777/100
        +0o200/666
        1+1i
        1-1i
        0x1f+2ei
        +0x1f-2ei
        0b10+11i
        -0b10-11i
      |}
  in
  token lexbuf;

  [%expect
    {|
    Dec 123
    Dec +123
    Dec -123
    Bin 01010101
    Bin -11110000
    Bin +11111111
    Oct 12345670
    Oct +76543210
    Oct -17263540
    Hex 123abcdef
    Hex -456DEFabc
    Hex +789ABCdef
    Dec 123/456
    Dec -456/789
    Dec +987/654
    Oct 777/100
    Oct +200/666
    Dec 1+1i
    Dec 1-1i
    Hex 1f+2ei
    Hex +1f-2ei
    Bin 10+11i
    Bin -10-11i
    EOF
    |}]
