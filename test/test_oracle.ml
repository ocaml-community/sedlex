open Sedlex_compiler
open Sedlex_oracle
module G = QCheck2.Gen

(* ================================================================== *)
(* Hand-written tests                                                 *)
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
  (* Rule 0 needs trailing 'c'; input "aaabb" backtracks to rule 1 *)
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

let%expect_test "complement" =
  (* Compl [a-c] matches any char NOT in a-c *)
  oracle [| lit 'd' ^. capture "x" (compl (cls 'a' 'c')) ^. lit 'd' |] "dxd";
  oracle [| lit 'd' ^. capture "x" (compl (cls 'a' 'c')) ^. lit 'd' |] "dad";
  [%expect {|
    "dxd" -> rule 0, len 3, [x="x"]
    "dad" -> no match
    |}]

let%expect_test "subtraction" =
  (* Sub([a-e], [c-e]) = [a-b] *)
  oracle [| capture "x" (plus (sub (cls 'a' 'e') (cls 'c' 'e'))) |] "abcde";
  [%expect {| "abcde" -> rule 0, len 2, [x="ab"] |}]

let%expect_test "intersection" =
  (* Inter([a-d], [c-f]) = [c-d] *)
  oracle [| capture "x" (plus (inter (cls 'a' 'd') (cls 'c' 'f'))) |] "cdabe";
  [%expect {| "cdabe" -> rule 0, len 2, [x="cd"] |}]

let%expect_test "or-pattern with discriminator" =
  (* (lit 'a' as x) | (lit 'b' as x) — each branch binds "x" with different tags *)
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
  (* (Plus 'a' as x) | (lit 'b', Plus 'c' as x) *)
  oracle
    [|
      alt
        (capture "x" (plus (lit 'a')))
        (capture "x" (seq (lit 'b') (plus (lit 'c'))));
    |]
    "aaa";
  oracle
    [|
      alt
        (capture "x" (plus (lit 'a')))
        (capture "x" (seq (lit 'b') (plus (lit 'c'))));
    |]
    "bccc";
  [%expect
    {|
    "aaa" -> rule 0, len 3, [x="aaa"]
    "bccc" -> rule 0, len 4, [x="bccc"]
    |}]

let%expect_test "known limitation: overlapping star and capture" =
  (* When Star and the following capture overlap in character sets,
     the DFA's epsilon closure (Sedlex.add_node) visits the bind's
     start-tag node via the star's loop first, preventing the capture's
     own path from recording its tag. See the FIXME on add_node.

     The capture must be variable-length and sandwiched between other
     parts of the pattern so that Start_plus/End_minus optimizations
     cannot resolve positions without tags. *)

  (* Disjoint char sets: works correctly *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'b')) ^. lit 'c' |] "aabbc";
  (* Overlapping char sets: star and capture compete for 'a' *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) ^. lit 'b' |] "ab";
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) ^. lit 'b' |] "aaab";
  (* Plus has the same issue *)
  oracle [| plus (lit 'a') ^. capture "x" (plus (lit 'a')) ^. lit 'b' |] "aab";
  [%expect
    {|
    "aabbc" -> rule 0, len 5, [x="bb"]
    ERROR "ab" -> ocamllex=rule 0, len 2, [x="a"] dfa=rule 0, len 2, [x=""]
    ERROR "aaab" -> ocamllex=rule 0, len 4, [x="a"] dfa=rule 0, len 4, [x=""]
    ERROR "aab" -> ocamllex=rule 0, len 3, [x="a"] dfa=rule 0, len 3, [x=""]
    |}]

let qcheck_test ~name gen =
  let test =
    QCheck2.Test.make ~count:50_000 ~max_fail:10 ~name
      ~print:(fun (descs, input_str) ->
        Array.iteri
          (fun i d -> Printf.printf "  rule%d: %s\n" i (Ir.show d))
          descs;
        oracle descs input_str;
        "")
      gen
      (fun (irs, input_str) -> check irs input_str)
  in
  try QCheck2.Test.check_exn test with QCheck2.Test.Test_fail _ -> ()

let%expect_test "qcheck: single rule" =
  let gen = G.map2 (fun r s -> ([| r |], s)) gen_ir gen_input in
  qcheck_test ~name:"single" gen;
  [%expect
    {|
      rule0: ((Plus ['a'-'c'] as x), Star 'a', 'a')
    ERROR "aaa" -> ocamllex=rule 0, len 3, [x="aa"] dfa=rule 0, len 3, [x="aaa"]
      rule0: ((Star ['a'-'c'] as y), 'a', Star 'a')
    ERROR "aa" -> ocamllex=rule 0, len 2, [y="a"] dfa=rule 0, len 2, [y="aa"]
      rule0: (Star ['a'-'c'], ((Plus 'a', 'a') as y))
    ERROR "aaaaa" -> ocamllex=rule 0, len 5, [y="aa"] dfa=rule 0, len 5, [y=""]
      rule0: ((Plus (Star 'a', 'd') as x), Plus 'd')
    ERROR "dd" -> ocamllex=rule 0, len 2, [x="d"] dfa=rule 0, len 2, [x="dd"]
      rule0: (Star (Rep(['a'-'c'], 0..0) | 'a'), (('a', 'a', ('a' | Star 'a')) as y))
    ERROR "aa" -> ocamllex=rule 0, len 2, [y="aa"] dfa=rule 0, len 2, [y=""]
      rule0: (((Star 'a' | 'c') as z), 'c', Star 'a', 'c', ['a'-'c'])
    ERROR "cca" -> ocamllex=rule 0, len 3, [z=""] dfa=rule 0, len 3, [z="c"]
      rule0: ((('a' | Rep(['a'-'c'], 0..0)) as y), Plus 'a')
    ERROR "a" -> ocamllex=rule 0, len 1, [y=""] dfa=rule 0, len 1, [y="a"]
      rule0: (Star Rep(['a'-'c'], 0..1), (Star ('a', 'a') as y), 'a')
    ERROR "a" -> ocamllex=rule 0, len 1, [y=""] dfa=rule 0, len 1, [y=<invalid 1..0>]
      rule0: ((eps | 'c'), (('a' | Plus 'c') as y), ('a' | Star 'a'))
    ERROR "cb" -> ocamllex=rule 0, len 1, [y="c"] dfa=rule 0, len 1, [y=""]
      rule0: (('a' | Star 'a'), (Star Rep(['a'-'c'], 0..1) | 'a'), (('b' | ('a', 'a')) as x))
    ERROR "b" -> ocamllex=rule 0, len 1, [x="b"] dfa=rule 0, len 1, [x=""]
    |}]

let%expect_test "qcheck: two rules" =
  let gen = G.map3 (fun a b s -> ([| a; b |], s)) gen_ir gen_ir gen_input in
  qcheck_test ~name:"two rules" gen;
  [%expect
    {|
      rule0: 'a'
      rule1: (Star (['a'-'c'], Star 'a'), ('a' as y), Star 'a', 'b')
    ERROR "ab" -> ocamllex=rule 1, len 2, [y="a"] dfa=rule 1, len 2, [y=<invalid 2..3>]
      rule0: 'a'
      rule1: ((Star 'a' as x), 'b', (Star 'a' as x))
    ERROR "b" -> ocamllex=rule 1, len 1, [x=""] dfa=rule 1, len 1, [x="", x=""]
      rule0: 'a'
      rule1: (Star 'a', (Plus 'a' as y))
    ERROR "aa" -> ocamllex=rule 1, len 2, [y="a"] dfa=rule 1, len 2, [y=""]
      rule0: ('d', Star Rep(['a'-'c'], 0..0), (('a' | Star 'a') as x), Plus ['a'-'c'], 'd')
      rule1: 'a'
    ERROR "dad" -> ocamllex=rule 0, len 3, [x=""] dfa=rule 0, len 3, [x="a"]
      rule0: ((('b', Star 'a', ['a'-'c'], 'a', Plus Star 'b') as x), 'b', ('a' | Star 'a'))
      rule1: 'a'
    ERROR "baab" -> ocamllex=rule 0, len 4, [x="baa"] dfa=rule 0, len 4, [x="baab"]
      rule0: 'a'
      rule1: (Star ['a'-'c'], 'b', (['a'-'c'] as y), 'b', Star 'a')
    ERROR "abab" -> ocamllex=rule 1, len 4, [y="a"] dfa=rule 1, len 4, [y=<invalid 4..5>]
      rule0: 'a'
      rule1: (Star 'd', ('c' as z), ['a'-'c'], 'c', ('c' as z))
    ERROR "dcacc" -> ocamllex=rule 1, len 5, [z="c"] dfa=rule 1, len 5, [z="c", z="c"]
      rule0: 'a'
      rule1: (('a' | Star 'a'), ('a' as x), Star 'a')
    ERROR "aaaaa" -> ocamllex=rule 1, len 5, [x="a"] dfa=rule 1, len 5, [x=<invalid 5..6>]
      rule0: 'a'
      rule1: ((Star ['a'-'c'] as z), Star 'a', 'b')
    ERROR "b" -> ocamllex=rule 1, len 1, [z=""] dfa=rule 1, len 1, [z="b"]
      rule0: 'a'
      rule1: (Star ['a'-'c'], ('b' as z), (('a', 'a') | ['a'-'c']))
    ERROR "ba" -> ocamllex=rule 1, len 2, [z="b"] dfa=rule 1, len 2, [z=<invalid 2..3>]
    |}]
