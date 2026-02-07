let%expect_test "simple string match" =
  (match%sedlex_test buf with "ab" | "de" -> () | _ -> ());
  [%expect
    {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'"];
      state0 -> state3 [label="'d'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state3 [label="3"];
      state3 -> state2 [label="'e'"];
    }
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
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'-'z'"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'a'-'z'"];
    }
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
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'0'-'9'"];
      state0 -> state2 [label="'a'"];
      state0 -> state4 [label="'d'"];
      state1 [label="1\n[rule 2]", shape=doublecircle];
      state1 -> state1 [label="'0'-'9'"];
      state2 [label="2"];
      state2 -> state3 [label="'b'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
      state4 [label="4"];
      state4 -> state5 [label="'e'"];
      state5 [label="5\n[rule 1]", shape=doublecircle];
    }
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
