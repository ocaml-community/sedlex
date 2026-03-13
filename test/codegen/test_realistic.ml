(* Realistic multi-rule lexer exercising many patterns simultaneously.
   Current: init_mem 2.
   - Rule 0: (Plus 'a'..'z' as ns), '.', (Plus 'a'..'z' as name) → 1 tag
     (ns: start=0, end=tag0; name: start=tag0+1 via Tag offset, end=lexeme_length)
   - Rule 1: (Plus 'A'..'Z' as label), '=', (Plus '0'..'9' as value) → 1 tag
     (label: start=0, end=tag1; value: start=tag1+1 via Tag offset, end=lexeme_length)
   - Rule 2: "0x", (Plus hex as hex), ';' → 0 tags (prefix=2, suffix=1)
   - Rule 3: '(', ('a'..'z' as x), ',', ('a'..'z' as y), ')' → 0 tags (all fixed offsets)
   - Rule 4: (Plus digits as tok) | (Plus letters as tok) → 0 tags (discriminator elided)
   Remaining optimization goals:
   - Self-loop tag delay → rules 0,1 use Set_prev (O(1) instead of O(n))
   - Cross-rule cell sharing → rules 0,1 share cells (init_mem 2 → 1) *)
let%expect_test "realistic: multi-token lexer" =
  (match%sedlex_test buf with
    | (Plus 'a' .. 'z' as ns), '.', (Plus 'a' .. 'z' as name) ->
        ignore (ns, name)
    | (Plus 'A' .. 'Z' as label), '=', (Plus '0' .. '9' as value) ->
        ignore (label, value)
    | "0x", (Plus ('0' .. '9' | 'a' .. 'f') as hex), ';' -> ignore hex
    | '(', ('a' .. 'z' as x), ',', ('a' .. 'z' as y), ')' -> ignore (x, y)
    | (Plus '0' .. '9' as tok) | (Plus 'a' .. 'z' as tok) -> ignore tok
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
      state0 -> state1 [label="'('"];
      state0 -> state6 [label="'0'"];
      state0 -> state7 [label="'1'-'9'"];
      state0 -> state11 [label="'A'-'Z' {t1}"];
      state0 -> state14 [label="'a'-'z' {t0}"];
      state1 [label="1"];
      state1 -> state2 [label="'a'-'z'"];
      state2 [label="2"];
      state2 -> state3 [label="','"];
      state3 [label="3"];
      state3 -> state4 [label="'a'-'z'"];
      state4 [label="4"];
      state4 -> state5 [label="')'"];
      state5 [label="5\n[rule 3]", shape=doublecircle];
      state6 [label="6\n[rule 4]", shape=doublecircle];
      state6 -> state7 [label="'0'-'9'"];
      state6 -> state8 [label="'x'"];
      state7 [label="7\n[rule 4]", shape=doublecircle];
      state7 -> state7 [label="'0'-'9'"];
      state8 [label="8"];
      state8 -> state9 [label="'0'-'9', 'a'-'f'"];
      state9 [label="9"];
      state9 -> state9 [label="'0'-'9', 'a'-'f'"];
      state9 -> state10 [label="';'"];
      state10 [label="10\n[rule 2]", shape=doublecircle];
      state11 [label="11"];
      state11 -> state12 [label="'='"];
      state11 -> state11 [label="'A'-'Z' {t1}"];
      state12 [label="12"];
      state12 -> state13 [label="'0'-'9'"];
      state13 [label="13\n[rule 1]", shape=doublecircle];
      state13 -> state13 [label="'0'-'9'"];
      state14 [label="14\n[rule 4]", shape=doublecircle];
      state14 -> state15 [label="'.'"];
      state14 -> state14 [label="'a'-'z' {t0}"];
      state15 [label="15"];
      state15 -> state16 [label="'a'-'z'"];
      state16 [label="16\n[rule 0]", shape=doublecircle];
      state16 -> state16 [label="'a'-'z'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_6 buf
      | 2 -> __sedlex_state_7 buf
      | 3 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_11 buf)
      | 4 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_14 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_4 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> Sedlexing.accept buf 3
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_6 buf =
      Sedlexing.mark buf 4;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_7 buf
       | 1 -> __sedlex_state_8 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_7 buf =
      Sedlexing.mark buf 4;
      (match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_7 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_8 buf =
      match __sedlex_partition_7 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_9 buf =
      match __sedlex_partition_8 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 buf
      | 1 -> Sedlexing.accept buf 2
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_11 buf =
      match __sedlex_partition_9 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_12 buf
      | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_11 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_12 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_13 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_13 buf =
      Sedlexing.mark buf 1;
      (match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_13 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_14 buf =
      Sedlexing.mark buf 4;
      (match __sedlex_partition_10 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_15 buf
       | 1 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_14 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_15 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_16 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_16 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_16 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let ns =
          let __s = 0 in
          let __e = Sedlexing.__private__mem_pos buf 0 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let name =
          let __s = (Sedlexing.__private__mem_pos buf 0) + 1 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (ns, name)
    | 1 ->
        let label =
          let __s = 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let value =
          let __s = (Sedlexing.__private__mem_pos buf 1) + 1 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (label, value)
    | 2 ->
        let hex =
          let __s = 2 in
          let __e = (Sedlexing.lexeme_length buf) - 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore hex
    | 3 ->
        let x =
          let __s = 1 in
          let __e = (Sedlexing.lexeme_length buf) - 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = 3 in
          let __e = (Sedlexing.lexeme_length buf) - 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | 4 ->
        let tok =
          let __s = 0 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore tok
    | _ -> ()
    |}]
