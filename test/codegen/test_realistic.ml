(* Realistic multi-rule lexer exercising many patterns simultaneously.
   Current baseline (no optimizations): init_mem 22.
   - Rule 0: (Plus 'a'..'z' as ns), '.', (Plus 'a'..'z' as name) → 4 tags
   - Rule 1: (Plus 'A'..'Z' as label), '=', (Plus '0'..'9' as value) → 4 tags
   - Rule 2: "0x", (Plus hex as hex), ';' → 2 tags
   - Rule 3: '(', ('a'..'z' as x), ',', ('a'..'z' as y), ')' → 4 tags
   - Rule 4: (Plus digits as tok) | (Plus letters as tok) → 8 tags (or-pattern)
   Optimization goals:
   - Prefix/suffix offsets → rules 2,3 need 0 tags
   - Or-pattern discriminator elision → rule 4 needs 0 tags
   - Self-loop tag delay → rules 0,1 use Set_prev
   - Cross-rule cell sharing → rules 0,1 share cells
   - Adjacent gap elimination → rules 0,1 share end/start tags
   Optimized goal: init_mem 1. *)
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
      state0 -> state1 [label="'(' {t10}"];
      state0 -> state6 [label="'0' {d18=0,t15}"];
      state0 -> state7 [label="'1'-'9' {d18=0,t15}"];
      state0 -> state11 [label="'A'-'Z' {t5}"];
      state0 -> state14 [label="'a'-'z' {t1,d18=1,t17}"];
      state1 [label="1"];
      state1 -> state2 [label="'a'-'z' {t11}"];
      state2 [label="2"];
      state2 -> state3 [label="',' {t12}"];
      state3 [label="3"];
      state3 -> state4 [label="'a'-'z' {t13}"];
      state4 [label="4"];
      state4 -> state5 [label="')'"];
      state5 [label="5\n[rule 3]", shape=doublecircle];
      state6 [label="6\n[rule 4]", shape=doublecircle];
      state6 -> state7 [label="'0'-'9' {d18=0,t15}"];
      state6 -> state8 [label="'x' {t8}"];
      state7 [label="7\n[rule 4]", shape=doublecircle];
      state7 -> state7 [label="'0'-'9' {d18=0,t15}"];
      state8 [label="8"];
      state8 -> state9 [label="'0'-'9', 'a'-'f' {t9}"];
      state9 [label="9"];
      state9 -> state9 [label="'0'-'9', 'a'-'f' {t9}"];
      state9 -> state10 [label="';'"];
      state10 [label="10\n[rule 2]", shape=doublecircle];
      state11 [label="11"];
      state11 -> state12 [label="'=' {t6}"];
      state11 -> state11 [label="'A'-'Z' {t5}"];
      state12 [label="12"];
      state12 -> state13 [label="'0'-'9' {t7}"];
      state13 [label="13\n[rule 1]", shape=doublecircle];
      state13 -> state13 [label="'0'-'9' {t7}"];
      state14 [label="14\n[rule 4]", shape=doublecircle];
      state14 -> state15 [label="'.' {t2}"];
      state14 -> state14 [label="'a'-'z' {t1,d18=1,t17}"];
      state15 [label="15"];
      state15 -> state16 [label="'a'-'z' {t3}"];
      state16 [label="16\n[rule 0]", shape=doublecircle];
      state16 -> state16 [label="'a'-'z' {t3}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 10; __sedlex_state_1 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_value buf 18 0;
           Sedlexing.__private__set_mem_pos buf 15;
           __sedlex_state_6 buf)
      | 2 ->
          (Sedlexing.__private__set_mem_value buf 18 0;
           Sedlexing.__private__set_mem_pos buf 15;
           __sedlex_state_7 buf)
      | 3 -> (Sedlexing.__private__set_mem_pos buf 5; __sedlex_state_11 buf)
      | 4 ->
          (Sedlexing.__private__set_mem_pos buf 1;
           Sedlexing.__private__set_mem_value buf 18 1;
           Sedlexing.__private__set_mem_pos buf 17;
           __sedlex_state_14 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 11; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 12; __sedlex_state_3 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 13; __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> 3
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_6 buf =
      Sedlexing.mark buf 4;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 18 0;
            Sedlexing.__private__set_mem_pos buf 15;
            __sedlex_state_7 buf)
       | 1 -> (Sedlexing.__private__set_mem_pos buf 8; __sedlex_state_8 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_7 buf =
      Sedlexing.mark buf 4;
      (match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 18 0;
            Sedlexing.__private__set_mem_pos buf 15;
            __sedlex_state_7 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_8 buf =
      match __sedlex_partition_7 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 9; __sedlex_state_9 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_9 buf =
      match __sedlex_partition_8 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 9; __sedlex_state_9 buf)
      | 1 -> 2
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_11 buf =
      match __sedlex_partition_9 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 6; __sedlex_state_12 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 5; __sedlex_state_11 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_12 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 7; __sedlex_state_13 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_13 buf =
      Sedlexing.mark buf 1;
      (match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 7; __sedlex_state_13 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_14 buf =
      Sedlexing.mark buf 4;
      (match __sedlex_partition_10 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 2; __sedlex_state_15 buf)
       | 1 ->
           (Sedlexing.__private__set_mem_pos buf 1;
            Sedlexing.__private__set_mem_value buf 18 1;
            Sedlexing.__private__set_mem_pos buf 17;
            __sedlex_state_14 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_15 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_16 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_16 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_16 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 19;
          Sedlexing.__private__set_mem_pos buf 16;
          Sedlexing.__private__set_mem_pos buf 14;
          Sedlexing.__private__set_mem_pos buf 4;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let ns =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let name =
          let __s = Sedlexing.__private__mem_pos buf 2 in
          let __e = Sedlexing.__private__mem_pos buf 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (ns, name)
    | 1 ->
        let label =
          let __s = Sedlexing.__private__mem_pos buf 4 in
          let __e = Sedlexing.__private__mem_pos buf 5 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let value =
          let __s = Sedlexing.__private__mem_pos buf 6 in
          let __e = Sedlexing.__private__mem_pos buf 7 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (label, value)
    | 2 ->
        let hex =
          let __s = Sedlexing.__private__mem_pos buf 8 in
          let __e = Sedlexing.__private__mem_pos buf 9 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore hex
    | 3 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 10 in
          let __e = Sedlexing.__private__mem_pos buf 11 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 12 in
          let __e = Sedlexing.__private__mem_pos buf 13 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | 4 ->
        let tok =
          if (Sedlexing.__private__mem_value buf 18) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 14 in
            let __e = Sedlexing.__private__mem_pos buf 15 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 16 in
             let __e = Sedlexing.__private__mem_pos buf 17 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore tok
    | _ -> ()
    |}]
