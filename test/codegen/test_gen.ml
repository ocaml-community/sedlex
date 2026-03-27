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
    match Sedlexing.start buf; __sedlex_state_0 buf with | 0 -> () | _ -> ()
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
    match Sedlexing.start buf; __sedlex_state_0 buf with | 0 -> () | _ -> ()
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
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 -> ()
    | 1 -> ()
    | 2 -> ()
    | _ -> ()
    |}]

let%expect_test "as binding: simple" =
  (match%sedlex_test buf with 'a', ('b' as x), 'c' -> ignore x | _ -> ());
  [%expect
    {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

let%expect_test "as binding: whole-match shortcut" =
  (match%sedlex_test buf with Plus 'a' .. 'z' as x -> ignore x | _ -> ());
  [%expect
    {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'-'z' {t1}"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'a'-'z' {t1}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_1 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

let%expect_test "as binding: multiple bindings" =
  (match%sedlex_test buf with
    | ('a' as x), ('b' as y), 'c' -> ignore (x, y)
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
      state0 -> state1 [label="'a' {t2,t1}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t3}"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 4;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 2 in
          let __e = Sedlexing.__private__mem_pos buf 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | _ -> ()
    |}]

let%expect_test "as binding: or-pattern with discriminator" =
  (match%sedlex_test buf with
    | (Plus '0' .. '9' as x) | (Plus 'a' .. 'z' as x) -> ignore x
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
      state0 -> state1 [label="'0'-'9' {d4=0,t1}"];
      state0 -> state2 [label="'a'-'z' {d4=1,t3}"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'0'-'9' {d4=0,t1}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'a'-'z' {d4=1,t3}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_value buf 4 0;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_value buf 4 1;
           Sedlexing.__private__set_mem_pos buf 3;
           __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 4 0;
            Sedlexing.__private__set_mem_pos buf 1;
            __sedlex_state_1 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 4 1;
            Sedlexing.__private__set_mem_pos buf 3;
            __sedlex_state_2 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 5;
          Sedlexing.__private__set_mem_pos buf 2;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 4) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 2 in
             let __e = Sedlexing.__private__mem_pos buf 3 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore x
    | _ -> ()
    |}]

let%expect_test "as binding: shared prefix or-pattern" =
  (match%sedlex_test buf with
    | ("abc" as x), "def" | "a", ("bcd" as x), "ey" -> ignore x
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
      state0 -> state1 [label="'a' {t2}"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state3 [label="'c' {t1}"];
      state3 [label="3"];
      state3 -> state4 [label="'d' {t3}"];
      state4 [label="4"];
      state4 -> state5 [label="'e'"];
      state5 [label="5"];
      state5 -> state6 [label="'f' {d4=0}"];
      state5 -> state7 [label="'y' {d4=1}"];
      state6 [label="6\n[rule 0]", shape=doublecircle];
      state7 [label="7\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 2; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_3 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_5 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 4 0; 0)
      | 1 -> (Sedlexing.__private__set_mem_value buf 4 1; 0)
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 5;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 4) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 2 in
             let __e = Sedlexing.__private__mem_pos buf 3 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore x
    | _ -> ()
    |}]

let%expect_test "as binding: 3-way or reuses disc cell" =
  (match%sedlex_test buf with
    | ("ab" as x), "cd" | ("a" as x), "bce" | ("abc" as x), "df" -> ignore x
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
      state0 -> state1 [label="'a' {t3}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state3 [label="'c' {t6}"];
      state3 [label="3"];
      state3 -> state4 [label="'d' {d7=0,d4=0}"];
      state3 -> state6 [label="'e' {d7=0,d4=1}"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state4 -> state5 [label="'f' {d7=1}"];
      state5 [label="5\n[rule 0]", shape=doublecircle];
      state6 [label="6\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 6; __sedlex_state_3 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_value buf 7 0;
           Sedlexing.__private__set_mem_value buf 4 0;
           __sedlex_state_4 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_value buf 7 0;
           Sedlexing.__private__set_mem_value buf 4 1;
           0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_value buf 7 1; 0)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 8;
          Sedlexing.__private__set_mem_pos buf 5;
          Sedlexing.__private__set_mem_pos buf 2;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if
            ((Sedlexing.__private__mem_value buf 7) = 0) &&
              ((Sedlexing.__private__mem_value buf 4) = 0)
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            if
              ((Sedlexing.__private__mem_value buf 7) = 0) &&
                ((Sedlexing.__private__mem_value buf 4) = 1)
            then
              (let __s = Sedlexing.__private__mem_pos buf 2 in
               let __e = Sedlexing.__private__mem_pos buf 3 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
            else
              (let __s = Sedlexing.__private__mem_pos buf 5 in
               let __e = Sedlexing.__private__mem_pos buf 6 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore x
    | _ -> ()
    |}]

let%expect_test "as binding: multi-rule" =
  (match%sedlex_test buf with
    | 'a', ('b' as x) -> ignore x
    | "cd" -> ()
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
      state0 -> state1 [label="'a' {t0}"];
      state0 -> state3 [label="'c'"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state3 [label="3"];
      state3 -> state4 [label="'d'"];
      state4 [label="4\n[rule 1]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; 0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | 1 -> ()
    | _ -> ()
    |}]

let%expect_test "as binding: wrapping alternation" =
  (match%sedlex_test buf with 'x', (('a' | 'b') as y) -> ignore y | _ -> ());
  [%expect
    {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'x' {t0}"];
      state1 [label="1"];
      state1 -> state2 [label="'a'-'b' {t1}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; 0)
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let y =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore y
    | _ -> ()
    |}]

(* === Optimization tracking tests ===
   Each test below targets a specific optimization.
   As optimizations are implemented, the expected output will improve
   (fewer tags, fewer set_mem calls, etc.). *)

(* Optimization 1: Element-length (Offset_from_tag)
   When neither prefix nor suffix length is known but the element itself
   has a fixed codepoint length, only 1 tag should be needed instead of 2.
   Current: init_mem 2 (start + end tags).
   Goal: init_mem 1, with end computed as start_tag + 1. *)
let%expect_test "optim: element-length (Offset_from_tag)" =
  (match%sedlex_test buf with
    | Plus 'a', ('b' as x), Plus 'c' -> ignore x
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
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0}"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
      state3 -> state3 [label="'c'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_3 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 2: Or-pattern offset propagation
   When an or-pattern is at the top level of a rule, as-bindings
   covering the whole branch should get Start_plus 0 / End_minus 0
   without allocating any tags.
   Current: init_mem 5 (2 binding tags per branch + 1 disc cell).
   Goal: init_mem 0 (or no init_mem at all). *)
let%expect_test "optim: or-pattern offset propagation" =
  (match%sedlex_test buf with
    | (Plus 'a' as x) | (Plus 'b' as x) -> ignore x
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
      state0 -> state1 [label="'a' {d4=0,t1}"];
      state0 -> state2 [label="'b' {d4=1,t3}"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'a' {d4=0,t1}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'b' {d4=1,t3}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_value buf 4 0;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_value buf 4 1;
           Sedlexing.__private__set_mem_pos buf 3;
           __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 4 0;
            Sedlexing.__private__set_mem_pos buf 1;
            __sedlex_state_1 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 4 1;
            Sedlexing.__private__set_mem_pos buf 3;
            __sedlex_state_2 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 5;
          Sedlexing.__private__set_mem_pos buf 2;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 4) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 2 in
             let __e = Sedlexing.__private__mem_pos buf 3 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore x
    | _ -> ()
    |}]

(* Optimization 3: Discriminator elision
   When both branches of an or-pattern produce identical position
   expressions, the discriminator tag should be skipped entirely.
   Current: init_mem 5 (same as optim 2).
   Goal: 0 tags (both branches yield Start_plus 0, End_minus 0). *)
let%expect_test "optim: discriminator elision" =
  (match%sedlex_test buf with
    | (Plus '0' .. '9' as x) | (Plus 'a' .. 'z' as x) -> ignore x
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
      state0 -> state1 [label="'0'-'9' {d4=0,t1}"];
      state0 -> state2 [label="'a'-'z' {d4=1,t3}"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'0'-'9' {d4=0,t1}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'a'-'z' {d4=1,t3}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_value buf 4 0;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_value buf 4 1;
           Sedlexing.__private__set_mem_pos buf 3;
           __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 4 0;
            Sedlexing.__private__set_mem_pos buf 1;
            __sedlex_state_1 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 4 1;
            Sedlexing.__private__set_mem_pos buf 3;
            __sedlex_state_2 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 5;
          Sedlexing.__private__set_mem_pos buf 2;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 4) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 2 in
             let __e = Sedlexing.__private__mem_pos buf 3 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore x
    | _ -> ()
    |}]

(* Optimization 4: Intra-rule tag coalescing
   Tags with identical occurrence signatures should share one memory cell.
   Here x_end (t1) and y_start (t2) fire on the same transitions.
   Current: init_mem 4 (x_start=t0, x_end=t1, y_start=t2, y_end=t3).
   Goal: init_mem 3 (t1 and t2 coalesce → x_start, x_end=y_start, y_end). *)
let%expect_test "optim: intra-rule tag coalescing" =
  (match%sedlex_test buf with
    | (Plus 'a' as x), (Plus 'b' as y) -> ignore (x, y)
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
      state0 -> state1 [label="'a' {t2,t1}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t2,t1}"];
      state1 -> state2 [label="'b' {t3}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'b' {t3}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_2 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 4;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 2 in
          let __e = Sedlexing.__private__mem_pos buf 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | _ -> ()
    |}]

(* Coalescing: or-pattern where both branches bind x at the same DFA
   position (Plus 'b' after Plus 'a'). Each branch allocates its own
   start/end tags for x, but since x covers the same DFA transitions
   in both branches, its tags have identical occurrence signatures and
   could coalesce. y's tags differ (Plus 'c' vs Plus 'f' → different states).
   Current: init_mem 9 (4 binding tags per branch + 1 disc cell).
   Goal with coalescing: x's branch tags share cells, reducing total. *)
let%expect_test "coalescing: or-pattern with same-position bindings" =
  (match%sedlex_test buf with
    | Plus 'a', (Plus 'b' as x), (Plus 'c' as y)
    | Plus 'a', (Plus 'b' as x), (Plus 'f' as y) ->
        ignore (x, y)
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
      state0 -> state1 [label="'a' {t0,t4}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0,t4}"];
      state1 -> state2 [label="'b' {t2,t1,t6,t5}"];
      state2 [label="2"];
      state2 -> state2 [label="'b' {t2,t1,t6,t5}"];
      state2 -> state3 [label="'c' {d8=0,t3}"];
      state2 -> state4 [label="'f' {d8=1,t7}"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
      state3 -> state3 [label="'c' {d8=0,t3}"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state4 -> state4 [label="'f' {d8=1,t7}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 0;
           Sedlexing.__private__set_mem_pos buf 4;
           __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 0;
           Sedlexing.__private__set_mem_pos buf 4;
           __sedlex_state_1 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           Sedlexing.__private__set_mem_pos buf 6;
           Sedlexing.__private__set_mem_pos buf 5;
           __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           Sedlexing.__private__set_mem_pos buf 6;
           Sedlexing.__private__set_mem_pos buf 5;
           __sedlex_state_2 buf)
      | 1 ->
          (Sedlexing.__private__set_mem_value buf 8 0;
           Sedlexing.__private__set_mem_pos buf 3;
           __sedlex_state_3 buf)
      | 2 ->
          (Sedlexing.__private__set_mem_value buf 8 1;
           Sedlexing.__private__set_mem_pos buf 7;
           __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 8 0;
            Sedlexing.__private__set_mem_pos buf 3;
            __sedlex_state_3 buf)
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_4 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 ->
           (Sedlexing.__private__set_mem_value buf 8 1;
            Sedlexing.__private__set_mem_pos buf 7;
            __sedlex_state_4 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 9;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 8) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 4 in
             let __e = Sedlexing.__private__mem_pos buf 5 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        let y =
          if (Sedlexing.__private__mem_value buf 8) = 0
          then
            let __s = Sedlexing.__private__mem_pos buf 2 in
            let __e = Sedlexing.__private__mem_pos buf 3 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = Sedlexing.__private__mem_pos buf 6 in
             let __e = Sedlexing.__private__mem_pos buf 7 in
             { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore (x, y)
    | _ -> ()
    |}]

(* Optimization 5: Cross-rule cell sharing (graph coloring)
   Non-interfering rules should reuse the same memory cells.
   Rule 0 and rule 1 never co-exist in the same DFA state (beyond state 0),
   so their tags can share cells.
   Current: init_mem 4 (2 per rule, summed).
   Goal: init_mem 2 (max of the two, cells shared). *)
let%expect_test "optim: cross-rule cell sharing" =
  (match%sedlex_test buf with
    | Plus 'a', (Plus 'b' as x), Plus 'c' -> ignore x
    | Plus 'd', (Plus 'e' as y), Plus 'f' -> ignore y
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
      state0 -> state1 [label="'a' {t0}"];
      state0 -> state4 [label="'d' {t2}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0}"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state2 [label="'b' {t1}"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
      state3 -> state3 [label="'c'"];
      state4 [label="4"];
      state4 -> state4 [label="'d' {t2}"];
      state4 -> state5 [label="'e' {t3}"];
      state5 [label="5"];
      state5 -> state5 [label="'e' {t3}"];
      state5 -> state6 [label="'f'"];
      state6 [label="6\n[rule 1]", shape=doublecircle];
      state6 -> state6 [label="'f'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 2; __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | 1 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_3 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_4 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 2; __sedlex_state_4 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_5 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_5 buf)
      | 1 -> __sedlex_state_6 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_6 buf =
      Sedlexing.mark buf 1;
      (match __sedlex_partition_7 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_6 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 4;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | 1 ->
        let y =
          let __s = Sedlexing.__private__mem_pos buf 2 in
          let __e = Sedlexing.__private__mem_pos buf 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore y
    | _ -> ()
    |}]

(* Optimization 6: Dead tag elimination
   Tags on transitions where the tag's owning rule can no longer reach
   a final state should be removed.
   Rule 0 has a binding on Plus 'b'; rule 1 does not.
   Both share the Plus 'a', Plus 'b' prefix in the DFA.
   Current: init_mem 2, tags t0/t1 set on shared prefix transitions
   even when only rule 1 is reachable via 'd'.
   Goal: no tags on transitions leading exclusively to rule 1. *)
let%expect_test "optim: dead tag elimination" =
  (match%sedlex_test buf with
    | Plus 'a', (Plus 'b' as x), 'c' -> ignore x
    | Plus 'a', Plus 'b', 'd' -> ()
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
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0}"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state2 [label="'b' {t1}"];
      state2 -> state3 [label="'c'"];
      state2 -> state4 [label="'d'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
      state4 [label="4\n[rule 1]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | 1 -> 0
      | 2 -> 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | 1 -> ()
    | _ -> ()
    |}]

(* Optimization 7: Self-loop tag delay (Set_prev)
   Tags on a self-loop that also appear on all entering transitions
   should be delayed to exit transitions as Set_prev.
   Current: init_mem 2, set_mem t1 on every 'a' iteration (O(n)).
   Goal: no set_mem on the self-loop, set_mem_prev on exit (O(1)). *)
let%expect_test "optim: self-loop tag delay" =
  (match%sedlex_test buf with (Plus 'a' as x), Plus 'b' -> ignore x | _ -> ());
  [%expect
    {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a' {t1}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t1}"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'b'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_1 buf)
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 8: Tag remapping
   After coalescing and dead-tag elimination, the PPX should remap
   Tag references through the compiler's tag_map.
   Current: init_mem 6 (x: t0+t1, y: t2+t3, z: t4+t5).
   Goal: tested implicitly by coalescing — if remapping is wrong,
   the generated code will reference incorrect cell indices. *)
let%expect_test "optim: tag remapping after coalescing" =
  (match%sedlex_test buf with
    | ('a' as x), (Plus 'b' as y), ('c' as z) -> ignore (x, y, z)
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
      state0 -> state1 [label="'a' {t2,t1}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t4,t3}"];
      state2 [label="2"];
      state2 -> state2 [label="'b' {t4,t3}"];
      state2 -> state3 [label="'c' {t5}"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 4;
           Sedlexing.__private__set_mem_pos buf 3;
           __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 4;
           Sedlexing.__private__set_mem_pos buf 3;
           __sedlex_state_2 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 5; 0)
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 6;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 2 in
          let __e = Sedlexing.__private__mem_pos buf 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let z =
          let __s = Sedlexing.__private__mem_pos buf 4 in
          let __e = Sedlexing.__private__mem_pos buf 5 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y, z)
    | _ -> ()
    |}]

(* Optimization 9: Set_prev with backtracking
   Opt at the end means the DFA can accept at two states (with or without
   the optional 'a'). When self-loop tag delay is implemented, the delayed
   tags (Set_prev) must survive mark/backtrack correctly.
   Current: init_mem 4 (x: t0+t1, y: t2+t3), set_mem on every iteration. *)
let%expect_test "optim: set_prev with backtracking" =
  (match%sedlex_test buf with
    | (Plus 'a' as x), ((Plus 'b', Opt 'a') as y) -> ignore (x, y)
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
      state0 -> state1 [label="'a' {t2,t1}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t2,t1}"];
      state1 -> state2 [label="'b' {t3}"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state3 [label="'a' {t3}"];
      state2 -> state2 [label="'b' {t3}"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 3; 0)
       | 1 -> (Sedlexing.__private__set_mem_pos buf 3; __sedlex_state_2 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 4;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 2 in
          let __e = Sedlexing.__private__mem_pos buf 3 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | _ -> ()
    |}]

let%expect_test "Rep fixed-length prefix enables Start_plus" =
  (* Rep('0'..'9', 3..3) has fixed length 3.
     Current: init_mem 2 (start + end tags for x).
     Goal: 0 tags (prefix=3, suffix=0 both known → Start_plus/End_minus). *)
  (match%sedlex_test buf with
    | Rep ('0' .. '9', 3 .. 3), (Plus 'a' .. 'z' as x) -> ignore x
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
      state1 [label="1"];
      state1 -> state2 [label="'0'-'9'"];
      state2 [label="2"];
      state2 -> state3 [label="'0'-'9' {t0}"];
      state3 [label="3"];
      state3 -> state4 [label="'a'-'z' {t1}"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state4 -> state4 [label="'a'-'z' {t1}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_3 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

let%expect_test "as binding: or-chain then nested or on right" =
  (match%sedlex_test buf with
    | ("ab" as x), ("ef" as y)
    | ("a" as x), ("bef" as y)
    | (("cde" as x), "f" | ("c" as x), "ef"), ("gh" as y) ->
        ignore (x, y)
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
      state0 -> state1 [label="'a' {t6,t5}"];
      state0 -> state5 [label="'c' {t12}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t2,t1}"];
      state2 [label="2"];
      state2 -> state3 [label="'e'"];
      state3 [label="3"];
      state3 -> state4 [label="'f' {d8=0,t3,d16=0,d8=1,t7}"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state5 [label="5"];
      state5 -> state6 [label="'d'"];
      state5 -> state11 [label="'e'"];
      state6 [label="6"];
      state6 -> state7 [label="'e' {t10}"];
      state7 [label="7"];
      state7 -> state8 [label="'f' {t14,d13=0}"];
      state8 [label="8"];
      state8 -> state9 [label="'g'"];
      state9 [label="9"];
      state9 -> state10 [label="'h' {d16=1,t15}"];
      state10 [label="10\n[rule 0]", shape=doublecircle];
      state11 [label="11"];
      state11 -> state12 [label="'f' {t14,d13=1}"];
      state12 [label="12"];
      state12 -> state9 [label="'g'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 6;
           Sedlexing.__private__set_mem_pos buf 5;
           __sedlex_state_1 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 12; __sedlex_state_5 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 2;
           Sedlexing.__private__set_mem_pos buf 1;
           __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_value buf 8 0;
           Sedlexing.__private__set_mem_pos buf 3;
           Sedlexing.__private__set_mem_value buf 16 0;
           Sedlexing.__private__set_mem_value buf 8 1;
           Sedlexing.__private__set_mem_pos buf 7;
           0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_6 buf
      | 1 -> __sedlex_state_11 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_6 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 10; __sedlex_state_7 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_7 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 14;
           Sedlexing.__private__set_mem_value buf 13 0;
           __sedlex_state_8 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_8 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_9 buf =
      match __sedlex_partition_7 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_value buf 16 1;
           Sedlexing.__private__set_mem_pos buf 15;
           0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_11 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 ->
          (Sedlexing.__private__set_mem_pos buf 14;
           Sedlexing.__private__set_mem_value buf 13 1;
           __sedlex_state_12 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_12 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 buf
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 17;
          Sedlexing.__private__set_mem_pos buf 11;
          Sedlexing.__private__set_mem_pos buf 9;
          Sedlexing.__private__set_mem_pos buf 4;
          Sedlexing.__private__set_mem_pos buf 0;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if
            ((Sedlexing.__private__mem_value buf 16) = 0) &&
              ((Sedlexing.__private__mem_value buf 8) = 0)
          then
            let __s = Sedlexing.__private__mem_pos buf 0 in
            let __e = Sedlexing.__private__mem_pos buf 1 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            if
              ((Sedlexing.__private__mem_value buf 16) = 0) &&
                ((Sedlexing.__private__mem_value buf 8) = 1)
            then
              (let __s = Sedlexing.__private__mem_pos buf 4 in
               let __e = Sedlexing.__private__mem_pos buf 5 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
            else
              if
                ((Sedlexing.__private__mem_value buf 16) = 1) &&
                  ((Sedlexing.__private__mem_value buf 13) = 0)
              then
                (let __s = Sedlexing.__private__mem_pos buf 9 in
                 let __e = Sedlexing.__private__mem_pos buf 10 in
                 { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
              else
                (let __s = Sedlexing.__private__mem_pos buf 11 in
                 let __e = Sedlexing.__private__mem_pos buf 12 in
                 { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        let y =
          if
            ((Sedlexing.__private__mem_value buf 16) = 0) &&
              ((Sedlexing.__private__mem_value buf 8) = 0)
          then
            let __s = Sedlexing.__private__mem_pos buf 2 in
            let __e = Sedlexing.__private__mem_pos buf 3 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            if
              ((Sedlexing.__private__mem_value buf 16) = 0) &&
                ((Sedlexing.__private__mem_value buf 8) = 1)
            then
              (let __s = Sedlexing.__private__mem_pos buf 6 in
               let __e = Sedlexing.__private__mem_pos buf 7 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
            else
              (let __s = Sedlexing.__private__mem_pos buf 14 in
               let __e = Sedlexing.__private__mem_pos buf 15 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore (x, y)
    | _ -> ()
    |}]

let cond = true
let check_x (_x : Sedlexing.submatch) = true

let%expect_test "when guard: single rule" =
  (match%sedlex_test buf with 'a' when cond -> () | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> if cond then 0 else Sedlexing.backtrack buf
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with | 0 -> () | _ -> ()
    |}]

let%expect_test "when guard: overlapping rules" =
  (match%sedlex_test buf with
    | "ab" when cond -> ()
    | "ab" -> ()
    | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0,1]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> if cond then 0 else 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 -> ()
    | 1 -> ()
    | _ -> ()
    |}]

let%expect_test "when guard: shared accepting state" =
  (match%sedlex_test buf with
    | 'a' .. 'z' when cond -> ()
    | 'a' .. 'z' -> ()
    | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'-'z'"];
      state1 [label="1\n[rule 0,1]", shape=doublecircle];
    }
    CODE:
    let __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> if cond then 0 else 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 -> ()
    | 1 -> ()
    | _ -> ()
    |}]

let%expect_test "when guard: with mark" =
  (match%sedlex_test buf with
    | Plus 'a' .. 'z' when cond -> ()
    | Plus 'a' .. 'z' -> ()
    | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'-'z'"];
      state1 [label="1\n[rule 0,1]", shape=doublecircle];
      state1 -> state1 [label="'a'-'z'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      if cond then Sedlexing.mark buf 0 else Sedlexing.mark buf 1;
      (match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_1 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 -> ()
    | 1 -> ()
    | _ -> ()
    |}]

let%expect_test "when guard: unguarded rule between guarded rules" =
  (match%sedlex_test buf with
    | 'a' .. 'z' when cond -> ()
    | 'a' .. 'z' -> ()
    | 'a' .. 'z' when cond -> ()
    | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a'-'z'"];
      state1 [label="1\n[rule 0,1,2]", shape=doublecircle];
    }
    CODE:
    let __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> if cond then 0 else 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 -> ()
    | 1 -> ()
    | 2 -> ()
    | _ -> ()
    |}]

let%expect_test "when guard: with as binding" =
  (match%sedlex_test buf with
    | ('a', ('b' as x), 'c') when check_x x -> ()
    | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 ->
          if
            let x =
              let __s = Sedlexing.__private__mem_pos buf 0 in
              let __e = Sedlexing.__private__mem_pos buf 1 in
              { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
            check_x x
          then 0
          else Sedlexing.backtrack buf
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ()
    | _ -> ()
    |}]

let%expect_test "when guard: as binding with fallthrough" =
  (match%sedlex_test buf with
    | ('a', ('b' as x), 'c') when check_x x -> ignore x
    | "abc" -> ()
    | _ -> ());
  [%expect {|
    DOT:
    digraph {
      rankdir=LR;
      node [shape=circle];

      _start [shape=point];
      _start -> state0;

      state0 [label="0"];
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state2 [label="'b' {t1}"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0,1]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_2 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 ->
          if
            let x =
              let __s = Sedlexing.__private__mem_pos buf 0 in
              let __e = Sedlexing.__private__mem_pos buf 1 in
              { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
            check_x x
          then 0
          else 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | 1 -> ()
    | _ -> ()
    |}]
