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
    ERROR "aa" -> ref=rule 0, len 2, [z="a"] dfa=rule 0, len 2, [z="aa"]
    "aa" -> rule 0, len 2, [z="a"]
    "aaa" -> rule 0, len 3, [z="aa"]
    |}]

let%expect_test "Rep with even-parity tail" =
  (* plus(seq('a','a')) needs an even number of trailing chars: for "aaaaa"
     z=1 is the only repetition count that lets the tail match. *)
  oracle
    [| capture "z" (rep (lit 'a') 0 2) ^. plus (lit 'a' ^. lit 'a') |]
    "aaaaa";
  [%expect
    {| ERROR "aaaaa" -> ref=rule 0, len 5, [z="a"] dfa=rule 0, len 5, [z="aa"] |}]

let%expect_test "star/capture ambiguity (greedy spec)" =
  (* Greedy: the star takes as much as it can, the capture gets the rest. *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) |] "aaa";
  [%expect
    {| ERROR "aaa" -> ref=rule 0, len 3, [x="a"] dfa=rule 0, len 3, [x=""] |}]

(* ================================================================== *)
(* BUG: loop preceding a capture (single-register-vector miscompute)   *)
(* ================================================================== *)

let%expect_test "BUG: star before variable-length capture" =
  (* Unambiguous: the only complete parse is star="a", x="abb". The DFA's
     epsilon closure refires the capture's start tag on every star
     iteration, so the recorded start drifts to the last 'a'. *)
  oracle [| star (lit 'a') ^. capture "x" (lit 'a' ^. plus (lit 'b')) |] "aabb";
  [%expect
    {| ERROR "aabb" -> ref=rule 0, len 4, [x="abb"] dfa=rule 0, len 4, [x="bb"] |}]

let%expect_test "BUG: capture start refires past capture entry" =
  (* The start tag refires even on characters consumed INSIDE the capture
     (the star path stays alive in the DFA state), so x can come out as a
     value Plus 'a' cannot even match. *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) ^. lit 'b' |] "aab";
  [%expect
    {| ERROR "aab" -> ref=rule 0, len 3, [x="a"] dfa=rule 0, len 3, [x=""] |}]

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
(* Random sweeps (deterministic seed)                                  *)
(* ================================================================== *)

let%expect_test "qcheck: single rule" =
  qcheck (G.map2 (fun r s -> ([| r |], s)) gen_ir gen_input);
  [%expect
    {|
      rule0: ((Plus ['b'-'d'] as x), ['a'-'d'], Star ['c'-'d'])
    ERROR "bdc" -> ref=rule 0, len 3, [x="bd"] dfa=rule 0, len 3, [x="bdc"]
      rule0: ((Rep('b', 1..3) as x), (Plus 'b' as y))
    ERROR "bbac" -> ref=rule 0, len 2, [x="b", y="b"] dfa=rule 0, len 2, [x="bb", y=""]
      rule0: ((Star Star 'c' as x), Plus ['a'-'c'])
    ERROR "ccd" -> ref=rule 0, len 2, [x="c"] dfa=rule 0, len 2, [x="cc"]
      rule0: ((Plus ['b'-'d'] as x), 'b', Star 'c')
    ERROR "bba" -> ref=rule 0, len 2, [x="b"] dfa=rule 0, len 2, [x="bb"]
      rule0: (((Rep(['c'-'d'], 0..1), Plus 'a') as x), ((['a', 'd'] | ('a', 'd')) as y))
    ERROR "aabc" -> ref=rule 0, len 2, [x="a", y="a"] dfa=rule 0, len 2, [x="aa", y=""]
    5/2000 cases failed (printed at most 5)
    |}]

let%expect_test "qcheck: two rules" =
  qcheck ~count:1000
    (G.map3 (fun a b s -> ([| a; b |], s)) gen_ir gen_ir gen_input);
  [%expect
    {|
      rule0: ((Star Plus ['a'-'c'] as x), Plus Rep(['b'-'c'], 1..3))
      rule1: (('b' as x), 'a', ((Rep('a', 0..2), 'd') as z))
    ERROR "c" -> ref=rule 0, len 1, [x=""] dfa=rule 0, len 1, [x="c"]
      rule0: ('a' as x)
      rule1: (((Star ['c'-'d'], ['a'-'c']) as x), (['a', 'd'] | Rep(['b'-'d'], 1..2)), Star ['c'-'d'], Star (['a'-'d'], 'a'))
    ERROR "ccb" -> ref=rule 1, len 3, [x="cc"] dfa=rule 1, len 3, [x="ccb"]
      rule0: (Star 'd', (['a'-'d'] as y), ['a'-'d'], ((Star 'a' | 'b') as w))
      rule1: (['a'-'c'], ('a' as y))
    ERROR "ddd" -> ref=rule 0, len 3, [w="", y="d"] dfa=rule 0, len 3, [w=<5..3>, y=<3..4>]
      rule0: ((Star ['a'-'c'] as x), Plus ['a'-'b'], ['a'-'d'])
      rule1: (((('d', ['a'-'d']) | (['a'-'b'], ['b'-'d'])) as x), Star 'd', ('d' as z))
    ERROR "bd" -> ref=rule 0, len 2, [x=""] dfa=rule 0, len 2, [x="b"]
      rule0: (((Star ['a'-'d'] | Rep(['c'-'d'], 0..2)) as x), 'a', (Plus 'b' as z))
      rule1: ((Plus ['a'-'d'] as x), Plus ['a'-'d'], ('d' as z))
    ERROR "ccd" -> ref=rule 1, len 3, [x="c", z="d"] dfa=rule 1, len 3, [x="ccd", z="d"]
    6/1000 cases failed (printed at most 5)
    |}]
