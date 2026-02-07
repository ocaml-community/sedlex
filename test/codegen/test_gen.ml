let%expect_test "simple string match" =
  (match%sedlex_test buf with "ab" | "de" -> () | _ -> ());
  [%expect
    {|
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf in
    Sedlexing.start buf; (match __sedlex_state_0 buf with | 0 -> () | _ -> ())
    |}]

let%expect_test "character class" =
  (match%sedlex_test buf with Plus 'a' .. 'z' -> () | _ -> ());
  [%expect
    {|
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_1 buf
       | _ -> Sedlexing.backtrack buf) in
    Sedlexing.start buf; (match __sedlex_state_0 buf with | 0 -> () | _ -> ())
    |}]

let%expect_test "multi-rule" =
  (match%sedlex_test buf with
    | "ab" -> ()
    | "de" -> ()
    | Plus '0' .. '9' -> ()
    | _ -> ());
  [%expect
    {|
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_2 buf
      | 2 -> __sedlex_state_4 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 2;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_1 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> 1
      | _ -> Sedlexing.backtrack buf in
    Sedlexing.start buf;
    (match __sedlex_state_0 buf with | 0 -> () | 1 -> () | 2 -> () | _ -> ())
    |}]
