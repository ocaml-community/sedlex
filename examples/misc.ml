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
        Printf.printf "8. %s: %s %s\n" (lex buf) (sub a) (sub b);
        token buf
    (* {Others} *)
    | Plus xml_blank -> token buf
    | 128 .. 255 -> print_endline "Non ASCII"
    | eof -> print_endline "EOF"
    | _ -> failwith "Unexpected character"

let () =
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
      |}
  in
  token lexbuf
