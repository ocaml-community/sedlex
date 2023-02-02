let rec token buf =
  let lex buf = Sedlexing.Latin1.lexeme buf in
  let sub (a, b) = Sedlexing.Latin1.sub_lexeme buf a b in
  match%sedlex buf with
    | (((((("a" as a), "b") as b), "c") as c), "d") as d ->
        Printf.printf "1. %s: %s %s %s %s\n" (lex buf) (sub a) (sub b) (sub c)
          (sub d);
        token buf
    | ("d", (("c", (("b", ("a" as a)) as b)) as c)) as d ->
        Printf.printf "2. %s: %s %s %s %s\n" (lex buf) (sub a) (sub b) (sub c)
          (sub d);
        token buf
    | (('a' as a), ('b' as b as d) | ('b' as b), ('a' as a as d)) as c ->
        Printf.printf "3. %s: %s %s %s %s\n" (lex buf) (sub a) (sub b) (sub c)
          (sub d);
        token buf
    | (Plus "ab" as a), (Plus "ab" as b) ->
        Printf.printf "4. %s: %s %s\n" (lex buf) (sub a) (sub b);
        token buf
    | 'c', (Star "dc" as s), 'd' ->
        Printf.printf "5. %s: %s\n" (lex buf) (sub s);
        token buf
    | (Star "dcdc" as a), ("dcdc" as b) ->
        Printf.printf "6. %s: %s %s\n" (lex buf) (sub a) (sub b);
        token buf
    | "dc" as s ->
        Printf.printf "7. %s: %s\n" (lex buf) (sub s);
        token buf
    | ("abc" | "def") as s ->
        Printf.printf "8. %s: %s\n" (lex buf) (sub s);
        token buf
    | (Plus "a" as a), (Plus "b" as b), "c" ->
        Printf.printf "9. %s: %s %s\n" (lex buf) (sub a) (sub b);
        token buf
    | (Star "a" as a), (Plus "b" as b), "e"
    | (Plus "a" as b), (Plus "b" as a), "d" ->
        Printf.printf "10. %s: %s %s\n" (lex buf) (sub a) (sub b);
        token buf
    | ( ((Plus "d" as a) | (Plus "e" as a)),
        "f",
        ((Plus "d" as b) | (Plus "e" as b)) ) ->
        Printf.printf "11. %s: %s %s\n" (lex buf) (sub a) (sub b);
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
        abcd
        dcba
        ab
        ba
        ababababab
        cdcdcdcdcd
        dcdc
        dcdcdcdcdc
        abc
        def
        aaaabbbc
        aaaabbbbd
        aaaabbbbe
        dfe
        efd
        ddddfdddd
        eeeefeeee
      |}
  in
  token lexbuf;

  [%expect
    {|
    1. abcd: a ab abc abcd
    2. dcba: a ba cba dcba
    3. ab: a b ab b
    3. ba: a b ba a
    4. ababababab: ab abababab
    5. cdcdcdcdcd: dcdcdcdc
    6. dcdc:  dcdc
    6. dcdcdcdc: dcdc dcdc
    7. dc: dc
    8. abc: abc
    8. def: def
    9. aaaabbbc: aaaa bbb
    10. aaaabbbbd: bbbb aaaa
    10. aaaabbbbe: aaaa bbbb
    11. dfe: d e
    11. efd: e d
    11. ddddfdddd: dddd dddd
    11. eeeefeeee: eeee eeee
    EOF
    |}]
