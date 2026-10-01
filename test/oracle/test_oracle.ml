(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2026, Hugo Heuzard                                           *)

(* Oracle tests: the brute-force reference matcher vs the compiled DFA.
   Lines prefixed with ERROR mark disagreements, i.e. compiler bugs. The
   random sweeps print nothing when every case agrees. *)

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
     rule 1's captures. *)
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
(* Loop preceding a capture (#199)                                     *)
(* ================================================================== *)

let%expect_test "star before variable-length capture" =
  (* Unambiguous: the only complete parse is star="a", x="abb". The path
     still in the star and the path inside the capture share DFA states, so
     the capture's start must not follow the star's iterations. *)
  oracle [| star (lit 'a') ^. capture "x" (lit 'a' ^. plus (lit 'b')) |] "aabb";
  [%expect {| "aabb" -> rule 0, len 4, [x="abb"] |}]

let%expect_test "capture start after characters inside the capture" =
  (* The star path stays alive while characters are consumed inside the
     capture; x must still be a value Plus 'a' can match. *)
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) ^. lit 'b' |] "aab";
  [%expect {| "aab" -> rule 0, len 3, [x="a"] |}]

let%expect_test "star before capture with overlapping alt" =
  oracle
    [|
      star (lit 'a')
      ^. capture "y" (alt (lit 'a' ^. cls 'a' 'c') (star (cls 'a' 'c')));
    |]
    "aba";
  [%expect {| "aba" -> rule 0, len 3, [y="ba"] |}]

let%expect_test "stars around a single-char capture" =
  oracle [| star (lit 'a') ^. capture "y" (lit 'b') ^. star (lit 'b') |] "bb";
  [%expect {| "bb" -> rule 0, len 2, [y="b"] |}]

(* ================================================================== *)
(* End of input                                                        *)
(* ================================================================== *)

let%expect_test "capture before eof" =
  oracle [| capture "x" (star (lit 'a')) ^. eof |] "aa";
  oracle [| capture "x" (star (lit 'a')) ^. eof |] "";
  oracle [| capture "x" (star (lit 'a')) ^. eof |] "ab";
  [%expect
    {|
    "aa" -> rule 0, len 2, [x="aa"]
    "" -> rule 0, len 0, [x=""]
    "ab" -> no match
    |}]

let%expect_test "eof adds no length: rule priority decides" =
  oracle
    [| capture "x" (plus (lit 'a')); capture "y" (plus (lit 'a')) ^. eof |]
    "aa";
  oracle
    [| capture "x" (plus (lit 'a')) ^. eof; capture "y" (plus (lit 'a')) |]
    "aa";
  [%expect
    {|
    "aa" -> rule 0, len 2, [x="aa"]
    "aa" -> rule 0, len 2, [x="aa"]
    |}]

let%expect_test "eof tie within a rule" =
  (* Both branches match "a"; the left one wins. *)
  oracle [| alt (capture "x" (lit 'a')) (capture "x" (lit 'a' ^. eof)) |] "a";
  oracle [| alt (capture "x" (lit 'a' ^. eof)) (capture "x" (lit 'a')) |] "a";
  [%expect
    {|
    "a" -> rule 0, len 1, [x="a"]
    "a" -> rule 0, len 1, [x="a"]
    |}]

let%expect_test "eof is read once" =
  oracle [| capture "x" (plus eof) |] "";
  oracle [| capture "x" (star (alt (lit 'a') eof)) |] "aa";
  oracle [| capture "x" (eof ^. eof) |] "";
  [%expect
    {|
    "" -> rule 0, len 0, [x=""]
    "aa" -> rule 0, len 2, [x="aa"]
    "" -> no match
    |}]

(* ================================================================== *)
(* Random sweeps (deterministic seed)                                  *)
(* ================================================================== *)

let%expect_test "qcheck: single rule" =
  qcheck (G.map2 (fun r s -> ([| r |], s)) (gen_ir ~eof:true) gen_input);
  [%expect {| |}]

let%expect_test "qcheck: two rules" =
  qcheck ~count:1000
    (G.map3
       (fun a b s -> ([| a; b |], s))
       (gen_ir ~eof:true) (gen_ir ~eof:true) gen_input);
  [%expect {| |}]
