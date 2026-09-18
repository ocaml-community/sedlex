(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2026, Hugo Heuzard                                           *)

(* Oracle tests: the brute-force reference matcher vs the compiled DFA.
   Lines prefixed with ERROR mark disagreements — i.e. compiler bugs.
   The expect blocks record the CURRENT behavior; ERROR lines below are
   the known capture-position bug (single register vector per NFA node)
   and are expected to disappear with the TDFA rewrite. *)

open Oracle

(* ================================================================== *)
(* Hand-written: basic capture shapes                                  *)
(* ================================================================== *)

let%expect_test "simple capture" =
  oracle [| lit 'a' ^. capture "x" (plus (lit 'b')) ^. lit 'c' |] "abbc";
  [%expect {| "abbc" -> rule 0, len 4, [x="bb"] |}]

let%expect_test "whole-match capture" =
  oracle [| capture "x" (plus (cls 'a' 'z')) |] "hello";
  [%expect {| "hello" -> rule 0, len 5, [x="hello"] |}]

let%expect_test "two captures in sequence" =
  oracle
    [| capture "x" (plus (lit 'a')) ^. capture "y" (plus (lit 'b')) |]
    "aaabb";
  [%expect {| "aaabb" -> rule 0, len 5, [x="aaa", y="bb"] |}]

let%expect_test "variable-length capture" =
  oracle [| lit 'd' ^. capture "c" (star (cls 'a' 'c')) ^. lit 'd' |] "dabcd";
  [%expect {| "dabcd" -> rule 0, len 5, [c="abc"] |}]

let%expect_test "no match" =
  oracle [| lit 'a' ^. capture "x" (plus (lit 'b')) ^. lit 'c' |] "xyz";
  [%expect {| "xyz" -> no match |}]

let%expect_test "multi-rule" =
  oracle
    [|
      lit 'd' ^. capture "x" (plus (lit 'b')) ^. lit 'e';
      capture "x" (plus (cls 'a' 'c'));
    |]
    "dbbe";
  oracle
    [|
      lit 'd' ^. capture "x" (plus (lit 'b')) ^. lit 'e';
      capture "x" (plus (cls 'a' 'c'));
    |]
    "abc";
  [%expect
    {|
    "dbbe" -> rule 0, len 4, [x="bb"]
    "abc" -> rule 1, len 3, [x="abc"]
    |}]

let%expect_test "backtrack restores tags" =
  (* Rule 0 needs a trailing 'c'; on "aaabb" the DFA overruns into rule 0
     territory, fails, and must backtrack to rule 1's accepting state with
     rule 1's memory snapshot. *)
  oracle
    [|
      capture "x" (plus (lit 'a')) ^. plus (lit 'b') ^. lit 'c';
      capture "x" (plus (lit 'a')) ^. plus (lit 'b');
    |]
    "aaabb";
  [%expect {| "aaabb" -> rule 1, len 5, [x="aaa"] |}]

let%expect_test "bounded repetition" =
  oracle [| rep (lit 'a') 2 4 ^. capture "x" (plus (lit 'b')) |] "aaabbb";
  oracle [| rep (lit 'a') 2 4 ^. capture "x" (plus (lit 'b')) |] "abbb";
  [%expect
    {|
    "aaabbb" -> rule 0, len 6, [x="bbb"]
    "abbb" -> no match
    |}]

let%expect_test "complement, subtraction, intersection" =
  oracle [| lit 'd' ^. capture "x" (compl (cls 'a' 'c')) ^. lit 'd' |] "dxd";
  oracle [| lit 'd' ^. capture "x" (compl (cls 'a' 'c')) ^. lit 'd' |] "dad";
  oracle [| capture "x" (plus (sub (cls 'a' 'e') (cls 'c' 'e'))) |] "abcde";
  oracle [| capture "x" (plus (inter (cls 'a' 'd') (cls 'c' 'f'))) |] "cdabe";
  [%expect
    {|
    "dxd" -> rule 0, len 3, [x="x"]
    "dad" -> no match
    "abcde" -> rule 0, len 2, [x="ab"]
    "cdabe" -> rule 0, len 2, [x="cd"]
    |}]

let%expect_test "Rep(0,1) capture" =
  oracle [| capture "z" (opt (cls 'a' 'c')) ^. star (lit 'b') |] "b";
  oracle [| capture "z" (opt (cls 'a' 'c')) ^. star (lit 'b') |] "ab";
  oracle [| capture "z" (opt (cls 'a' 'c')) ^. star (lit 'b') |] "bb";
  [%expect
    {|
    "b" -> rule 0, len 1, [z="b"]
    "ab" -> rule 0, len 2, [z="a"]
    "bb" -> rule 0, len 2, [z="b"]
    |}]

(* ================================================================== *)
(* Or-patterns and discriminators                                      *)
(* ================================================================== *)

let%expect_test "or-pattern with discriminator" =
  oracle [| alt (capture "x" (lit 'a')) (capture "x" (lit 'b')) |] "a";
  oracle [| alt (capture "x" (lit 'a')) (capture "x" (lit 'b')) |] "b";
  oracle [| alt (capture "x" (lit 'a')) (capture "x" (lit 'b')) |] "c";
  [%expect
    {|
    "a" -> rule 0, len 1, [x="a"]
    "b" -> rule 0, len 1, [x="b"]
    "c" -> no match
    |}]

let%expect_test "or-pattern with variable-length branches" =
  let rules () =
    [|
      alt
        (capture "x" (plus (lit 'a')))
        (capture "x" (seq (lit 'b') (plus (lit 'c'))));
    |]
  in
  oracle (rules ()) "aaa";
  oracle (rules ()) "bccc";
  [%expect
    {|
    "aaa" -> rule 0, len 3, [x="aaa"]
    "bccc" -> rule 0, len 4, [x="bccc"]
    |}]

(* ================================================================== *)
(* Greedy disambiguation                                               *)
(* ================================================================== *)

let%expect_test "Rep greedy disambiguation" =
  (* Bounded repetition should greedily take as many iterations as the
     overall longest match allows. *)
  oracle [| capture "z" (rep (cls 'a' 'c') 0 2) ^. plus (lit 'a') |] "aaa";
  oracle [| capture "z" (rep (cls 'a' 'c') 0 2) ^. plus (lit 'a') |] "aa";
  oracle [| capture "z" (rep (cls 'a' 'c') 0 2) ^. lit 'a' |] "aa";
  oracle [| capture "z" (rep (cls 'a' 'c') 0 2) ^. lit 'a' |] "aaa";
  [%expect
    {|
    "aaa" -> rule 0, len 3, [z="aa"]
    "aa" -> rule 0, len 2, [z="a"]
    "aa" -> rule 0, len 2, [z="a"]
    "aaa" -> rule 0, len 3, [z="aa"]
    |}]

let%expect_test "Rep with even-parity tail" =
  (* plus(seq('a','a')) needs an even number of trailing chars: for "aaaaa"
     z=1 is the only repetition count that lets the tail match. *)
  oracle
    [| capture "z" (rep (lit 'a') 0 2) ^. plus (lit 'a' ^. lit 'a') |]
    "aaaaa";
  [%expect {| "aaaaa" -> rule 0, len 5, [z="a"] |}]

let%expect_test "star/capture ambiguity (greedy spec)" =
  (* Greedy: the star takes as much as it can, the capture gets the rest. *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) |] "aaa";
  [%expect {| "aaa" -> rule 0, len 3, [x="a"] |}]

(* ================================================================== *)
(* BUG: loop preceding a capture (single-register-vector miscompute)   *)
(* ================================================================== *)

let%expect_test "BUG: star before variable-length capture" =
  (* Unambiguous: the only complete parse is star="a", x="abb". The DFA's
     epsilon closure refires the capture's start tag on every star
     iteration, so the recorded start drifts to the last 'a'. *)
  oracle [| star (lit 'a') ^. capture "x" (lit 'a' ^. plus (lit 'b')) |] "aabb";
  [%expect {| "aabb" -> rule 0, len 4, [x="abb"] |}]

let%expect_test "BUG: capture start refires past capture entry" =
  (* The start tag refires even on characters consumed INSIDE the capture
     (the star path stays alive in the DFA state), so x can come out as a
     value Plus 'a' cannot even match. *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) ^. lit 'b' |] "aab";
  [%expect {| "aab" -> rule 0, len 3, [x="a"] |}]

let%expect_test "BUG: star before capture with overlapping alt" =
  oracle
    [|
      star (lit 'a')
      ^. capture "y" (alt (lit 'a' ^. cls 'a' 'c') (star (cls 'a' 'c')));
    |]
    "aba";
  [%expect {| "aba" -> rule 0, len 3, [y="ba"] |}]

let%expect_test "BUG: multiple stars before single-char capture" =
  oracle [| star (lit 'a') ^. capture "y" (lit 'b') ^. star (lit 'b') |] "bb";
  [%expect {| "bb" -> rule 0, len 2, [y="b"] |}]

(* ================================================================== *)
(* eof captures (regression for the fixed_length eof-width bug)         *)
(* ================================================================== *)

let%expect_test "capture spanning eof" =
  (* eof is zero-width: the End_minus offset must not retreat across it, so
     the capture keeps the whole preceding lexeme. *)
  oracle [| capture "x" (star any) ^. eof |] "abc";
  oracle [| capture "x" (lit 'a') ^. eof |] "a";
  oracle [| star any ^. capture "x" (opt (lit 'a')) ^. eof |] "b";
  [%expect
    {|
    "abc" -> rule 0, len 3, [x="abc"]
    "a" -> rule 0, len 1, [x="a"]
    "b" -> rule 0, len 1, [x=""]
    |}]

let%expect_test "capture before char-or-eof alternation" =
  (* '(a | eof)' is one cset of data-dependent width, so the capture before
     it must be tag-based, not offset-based. *)
  oracle [| capture "x" (lit 'x') ^. alt (lit 'a') eof |] "x";
  oracle [| capture "x" (lit 'x') ^. alt (lit 'a') eof |] "xa";
  [%expect
    {|
    "x" -> rule 0, len 1, [x="x"]
    "xa" -> rule 0, len 2, [x="x"]
    |}]

let%expect_test "control: capture before a real char (no eof)" =
  oracle [| capture "x" (lit 'a') ^. lit 'b' |] "ab";
  [%expect {| "ab" -> rule 0, len 2, [x="a"] |}]

let%expect_test "eof/rule-priority tie" =
  (* An earlier rule and a later eof-terminated rule match the same length; the
     earlier rule must win despite eof's zero-width mark landing later. *)
  let a = [| plus (alt (lit 'b') (lit 'c')); seq (star (cls 'a' 'c')) eof |] in
  oracle a "cc";
  oracle a "c";
  (* mirror: the eof-terminated rule is now first, so it wins the tie *)
  let b = [| seq (star (cls 'a' 'c')) eof; plus (alt (lit 'b') (lit 'c')) |] in
  oracle b "cc";
  [%expect
    {|
    "cc" -> rule 0, len 2
    "c" -> rule 0, len 1
    "cc" -> rule 0, len 2
    |}]

(* ================================================================== *)
(* Random sweeps (deterministic seed)                                  *)
(* ================================================================== *)

let%expect_test "qcheck: single rule" =
  qcheck (G.map2 (fun r s -> ([| r |], s)) (gen_ir ~eof:true ()) gen_input);
  [%expect {| |}]

let%expect_test "qcheck: two rules" =
  qcheck ~count:1000
    (G.map3
       (fun a b s -> ([| a; b |], s))
       (gen_ir ~eof:true ()) (gen_ir ~eof:true ()) gen_input);
  [%expect {| |}]
