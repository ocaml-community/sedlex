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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 1 in
          let __e = (Sedlexing.lexeme_length buf) - 1 in
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
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.lexeme_length buf in
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = (Sedlexing.lexeme_length buf) - 2 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = 1 in
          let __e = (Sedlexing.lexeme_length buf) - 1 in
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
      state0 -> state1 [label="'0'-'9'"];
      state0 -> state2 [label="'a'-'z'"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'0'-'9'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'a'-'z'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_1 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3"];
      state3 -> state4 [label="'d'"];
      state4 [label="4"];
      state4 -> state5 [label="'e'"];
      state5 [label="5"];
      state5 -> state6 [label="'f' {d0=0}"];
      state5 -> state7 [label="'y' {d0=1}"];
      state6 [label="6\n[rule 0]", shape=doublecircle];
      state7 [label="7\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
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
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_4 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_5 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 0 0; 0)
      | 1 -> (Sedlexing.__private__set_mem_value buf 0 1; 0)
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 0) = 0
          then
            let __s = 0 in
            let __e = (Sedlexing.lexeme_length buf) - 3 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            (let __s = 1 in
             let __e = (Sedlexing.lexeme_length buf) - 2 in
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3"];
      state3 -> state4 [label="'d' {d0=0}"];
      state3 -> state6 [label="'e' {d0=1}"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state4 -> state5 [label="'f' {d0=2}"];
      state5 [label="5\n[rule 0]", shape=doublecircle];
      state6 [label="6\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
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
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 0 0; __sedlex_state_4 buf)
      | 1 -> (Sedlexing.__private__set_mem_value buf 0 1; 0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_value buf 0 2; 0)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 0) = 0
          then
            let __s = 0 in
            let __e = (Sedlexing.lexeme_length buf) - 2 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            if (Sedlexing.__private__mem_value buf 0) = 1
            then
              (let __s = 0 in
               let __e = (Sedlexing.lexeme_length buf) - 3 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
            else
              (let __s = 0 in
               let __e = (Sedlexing.lexeme_length buf) - 2 in
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
      state0 -> state1 [label="'a'"];
      state0 -> state3 [label="'c'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state3 [label="3"];
      state3 -> state4 [label="'d'"];
      state4 [label="4\n[rule 1]", shape=doublecircle];
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
      | 0 -> 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 1 in
          let __e = Sedlexing.lexeme_length buf in
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
      state0 -> state1 [label="'x'"];
      state1 [label="1"];
      state1 -> state2 [label="'a'-'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> 0
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let y =
          let __s = 1 in
          let __e = Sedlexing.lexeme_length buf in
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
   Current: init_mem 1 (1 tag, end = tag + 1).
   Goal: init_mem 1 — already optimal. *)
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state1 [label="'a'"];
      state1 -> state2 [label="'b' {t0}"];
      state2 [label="2"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
      state3 -> state3 [label="'c'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_2 buf)
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
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = (Sedlexing.__private__mem_pos buf 0) + (-1) in
          let __e = Sedlexing.__private__mem_pos buf 0 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 1b: Deferred start tag past fixed-length prefix
   When an as-binding wraps a tuple that starts with fixed-length
   elements followed by variable-length, the start tag is placed
   after the prefix instead of at the very beginning.
   Example: ("0x", Plus hexa) as x — start tag fires after "0x". *)
let%expect_test "optim: deferred start tag past prefix" =
  (match%sedlex_test buf with
    | Plus 'a', (("0x", Plus ('0' .. '9' | 'a' .. 'f')) as x), Plus 'b' ->
        ignore x
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'0'"];
      state1 -> state1 [label="'a'"];
      state2 [label="2"];
      state2 -> state3 [label="'x' {t0}"];
      state3 [label="3"];
      state3 -> state4 [label="'0'-'9', 'a'-'f' {t1}"];
      state4 [label="4"];
      state4 -> state4 [label="'0'-'9', 'a', 'c'-'f' {t1}"];
      state4 -> state5 [label="'b' {t1}"];
      state5 [label="5\n[rule 0]", shape=doublecircle];
      state5 -> state4 [label="'0'-'9', 'a', 'c'-'f' {t1}"];
      state5 -> state5 [label="'b' {t1}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | 1 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_3 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_5 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
       | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_5 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = (Sedlexing.__private__mem_pos buf 0) + (-2) in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 1c: Nested alias anchor propagation
   When aliases are nested, the inner alias's tag positions should
   propagate as anchors to the outer alias.
   Example: ((Plus 'a' as x) as y) — y should reuse x's tags. *)
let%expect_test "optim: nested alias anchor propagation" =
  (match%sedlex_test buf with
    | ( Plus 'a',
        (("0x", (Plus ('0' .. '9' | 'a' .. 'f') as hex)) as full),
        Plus 'b' ) ->
        ignore (hex, full)
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'0'"];
      state1 -> state1 [label="'a'"];
      state2 [label="2"];
      state2 -> state3 [label="'x' {t0}"];
      state3 [label="3"];
      state3 -> state4 [label="'0'-'9', 'a'-'f' {t1}"];
      state4 [label="4"];
      state4 -> state4 [label="'0'-'9', 'a', 'c'-'f' {t1}"];
      state4 -> state5 [label="'b' {t1}"];
      state5 [label="5\n[rule 0]", shape=doublecircle];
      state5 -> state4 [label="'0'-'9', 'a', 'c'-'f' {t1}"];
      state5 -> state5 [label="'b' {t1}"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | 1 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_3 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
      | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_5 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
       | 0 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_4 buf)
       | 1 -> (Sedlexing.__private__set_mem_pos buf 1; __sedlex_state_5 buf)
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let full =
          let __s = (Sedlexing.__private__mem_pos buf 0) + (-2) in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let hex =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.__private__mem_pos buf 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (hex, full)
    | _ -> ()
    |}]

(* Optimization 2: Or-pattern offset propagation
   When an or-pattern is at the top level of a rule, as-bindings
   covering the whole branch should get Start_plus 0 / End_minus 0
   without allocating any tags.
   Current: 0 tags (offsets already known at top level).
   Goal: already optimal. *)
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
      state0 -> state1 [label="'a'"];
      state0 -> state2 [label="'b'"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'a'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'b'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_1 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 3: Discriminator elision
   When both branches of an or-pattern produce identical position
   expressions, the discriminator tag should be skipped entirely.
   Current: 0 tags (both branches yield Start_plus 0, End_minus 0).
   Goal: already optimal. *)
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
      state0 -> state1 [label="'0'-'9'"];
      state0 -> state2 [label="'a'-'z'"];
      state1 [label="1\n[rule 0]", shape=doublecircle];
      state1 -> state1 [label="'0'-'9'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'a'-'z'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_1 buf
       | _ -> Sedlexing.backtrack buf)
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 4: Intra-rule tag coalescing
   Tags with identical occurrence signatures should share one memory cell.
   Here x_end and y_start fire on the same transitions.
   Current: init_mem 1 (x_start=0, x_end=y_start via Tag offset, y_end=lexeme_length).
   Goal: init_mem 1 — already optimal. *)
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
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0}"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'b'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.__private__mem_pos buf 0 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | _ -> ()
    |}]

(* Optimization 5: Cross-rule cell sharing (graph coloring)
   Non-interfering rules should reuse the same memory cells.
   Rule 0 and rule 1 never co-exist in the same DFA state (beyond state 0),
   so their tags can share cells.
   Current: init_mem 4 (2 per rule: start + end tags for variable-length binding).
   Goal: init_mem 2 (cells shared across non-interfering rules). *)
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
   Current: init_mem 1, tag t0 set on shared prefix transitions
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
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state2 [label="'b'"];
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
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | 1 -> 0
      | 2 -> 1
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = (Sedlexing.lexeme_length buf) - 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | 1 -> ()
    | _ -> ()
    |}]

(* Optimization 7: Self-loop tag delay (Set_prev)
   Tags on a self-loop that also appear on all entering transitions
   should be delayed to exit transitions as Set_prev.
   Current: init_mem 1, set_mem t0 on every 'a' iteration (O(n)).
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
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0}"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state2 [label="'b'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.__private__mem_pos buf 0 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore x
    | _ -> ()
    |}]

(* Optimization 8: Tag remapping
   After coalescing and dead-tag elimination, the PPX should remap
   Tag references through the compiler's tag_map.
   Current: 0 tags (all offsets known: x=0..1, y=1..end-1, z=end-1..end).
   Goal: already optimal. *)
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
      state0 -> state1 [label="'a'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state2 [label="'b'"];
      state2 -> state3 [label="'c'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 buf
      | 1 -> 0
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = 1 in { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = 1 in
          let __e = (Sedlexing.lexeme_length buf) - 1 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let z =
          let __s = (Sedlexing.lexeme_length buf) - 1 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y, z)
    | _ -> ()
    |}]

(* Optimization 9: Set_prev with backtracking
   Opt at the end means the DFA can accept at two states (with or without
   the optional 'a'). When self-loop tag delay is implemented, the delayed
   tags (Set_prev) must survive mark/backtrack correctly.
   Current: init_mem 1 (x: start=0, end=tag0; y: start=tag0, end=lexeme_length). *)
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
      state0 -> state1 [label="'a' {t0}"];
      state1 [label="1"];
      state1 -> state1 [label="'a' {t0}"];
      state1 -> state2 [label="'b'"];
      state2 [label="2\n[rule 0]", shape=doublecircle];
      state2 -> state3 [label="'a'"];
      state2 -> state2 [label="'b'"];
      state3 [label="3\n[rule 0]", shape=doublecircle];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_1 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_pos buf 0; __sedlex_state_1 buf)
      | 1 -> __sedlex_state_2 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_2 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> 0
       | 1 -> __sedlex_state_2 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 1;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          let __s = 0 in
          let __e = Sedlexing.__private__mem_pos buf 0 in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        let y =
          let __s = Sedlexing.__private__mem_pos buf 0 in
          let __e = Sedlexing.lexeme_length buf in
          { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) } in
        ignore (x, y)
    | _ -> ()
    |}]

let%expect_test "Rep fixed-length prefix enables Start_plus" =
  (* Rep('0'..'9', 3..3) has fixed length 3.
     Current: 0 tags (prefix=3, suffix=0 both known → Start_plus/End_minus).
     Goal: already optimal. *)
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
      state2 -> state3 [label="'0'-'9'"];
      state3 [label="3"];
      state3 -> state4 [label="'a'-'z'"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state4 -> state4 [label="'a'-'z'"];
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
      | 0 -> __sedlex_state_3 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_3 buf =
      match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_4 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_4 buf =
      Sedlexing.mark buf 0;
      (match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
       | 0 -> __sedlex_state_4 buf
       | _ -> Sedlexing.backtrack buf) in
    match Sedlexing.start buf; __sedlex_state_0 buf with
    | 0 ->
        let x =
          let __s = 3 in
          let __e = Sedlexing.lexeme_length buf in
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
      state0 -> state1 [label="'a'"];
      state0 -> state5 [label="'c'"];
      state1 [label="1"];
      state1 -> state2 [label="'b'"];
      state2 [label="2"];
      state2 -> state3 [label="'e'"];
      state3 [label="3"];
      state3 -> state4 [label="'f' {d0=0}"];
      state4 [label="4\n[rule 0]", shape=doublecircle];
      state5 [label="5"];
      state5 -> state6 [label="'d'"];
      state5 -> state11 [label="'e'"];
      state6 [label="6"];
      state6 -> state7 [label="'e'"];
      state7 [label="7"];
      state7 -> state8 [label="'f' {d1=0}"];
      state8 [label="8"];
      state8 -> state9 [label="'g'"];
      state9 [label="9"];
      state9 -> state10 [label="'h' {d0=2}"];
      state10 [label="10\n[rule 0]", shape=doublecircle];
      state11 [label="11"];
      state11 -> state12 [label="'f' {d1=1}"];
      state12 [label="12"];
      state12 -> state9 [label="'g'"];
    }
    CODE:
    let rec __sedlex_state_0 buf =
      match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_1 buf
      | 1 -> __sedlex_state_5 buf
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
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 0 0; 0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_5 buf =
      match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_6 buf
      | 1 -> __sedlex_state_11 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_6 buf =
      match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_7 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_7 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 1 0; __sedlex_state_8 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_8 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 buf
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_9 buf =
      match __sedlex_partition_7 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 0 2; 0)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_11 buf =
      match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> (Sedlexing.__private__set_mem_value buf 1 1; __sedlex_state_12 buf)
      | _ -> Sedlexing.backtrack buf
    and __sedlex_state_12 buf =
      match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 buf
      | _ -> Sedlexing.backtrack buf in
    match Sedlexing.start buf;
          Sedlexing.__private__init_mem buf 2;
          __sedlex_state_0 buf
    with
    | 0 ->
        let x =
          if (Sedlexing.__private__mem_value buf 0) = 0
          then
            let __s = 0 in
            let __e = (Sedlexing.lexeme_length buf) - 2 in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            if (Sedlexing.__private__mem_value buf 0) = 1
            then
              (let __s = 0 in
               let __e = (Sedlexing.lexeme_length buf) - 3 in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
            else
              if
                ((Sedlexing.__private__mem_value buf 0) = 2) &&
                  ((Sedlexing.__private__mem_value buf 1) = 0)
              then
                (let __s = 0 in
                 let __e = (Sedlexing.lexeme_length buf) - 3 in
                 { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
              else
                (let __s = 0 in
                 let __e = (Sedlexing.lexeme_length buf) - 4 in
                 { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        let y =
          if (Sedlexing.__private__mem_value buf 0) = 0
          then
            let __s = 2 in
            let __e = Sedlexing.lexeme_length buf in
            { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }
          else
            if (Sedlexing.__private__mem_value buf 0) = 1
            then
              (let __s = 1 in
               let __e = Sedlexing.lexeme_length buf in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) })
            else
              (let __s = (Sedlexing.lexeme_length buf) - 2 in
               let __e = Sedlexing.lexeme_length buf in
               { Sedlexing.lexbuf = buf; pos = __s; len = (__e - __s) }) in
        ignore (x, y)
    | _ -> ()
    |}]
