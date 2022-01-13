open Printf

let next_tok buf =
  let open Sedlexing.Utf8 in
  match%sedlex buf with
    | "a", Utf8 (Chars "+-×÷") -> sprintf "with Chars: %s" (lexeme buf)
    | "b", Utf8 ("+" | "-" | "×" | "÷") ->
        sprintf "with or_pattern: %s" (lexeme buf)
    | _ -> failwith (sprintf "Unexpected character: %s" (lexeme buf))

let%expect_test _ =
  Sedlexing.Utf8.from_string "a+" |> next_tok |> print_string;
  [%expect {| with Chars: a+ |}];
  Sedlexing.Utf8.from_string "a÷" |> next_tok |> print_string;
  [%expect {| with Chars: a÷ |}];
  Sedlexing.Utf8.from_string "b+" |> next_tok |> print_string;
  [%expect {| with or_pattern: b+ |}];
  Sedlexing.Utf8.from_string "b÷" |> next_tok |> print_string;
  [%expect {| with or_pattern: b÷ |}]
