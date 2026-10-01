(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2026, Hugo Heuzard                                           *)

(* The reference matcher vs ocamllex on the same rules. *)

open Oracle_test.Oracle
open Ocamllex_oracle

let%expect_test "capture shapes" =
  oracle [| lit 'a' ^. capture "x" (plus (lit 'b')) ^. lit 'c' |] "abbc";
  oracle
    [| capture "x" (plus (lit 'a')) ^. capture "y" (plus (lit 'b')) |]
    "aaabb";
  oracle [| rep (lit 'a') 2 4 ^. capture "x" (plus (lit 'b')) |] "aaabbb";
  oracle [| alt (capture "x" (lit 'a')) (capture "x" (lit 'b')) |] "b";
  oracle
    [|
      capture "x" (plus (lit 'a')) ^. plus (lit 'b') ^. lit 'c';
      capture "x" (plus (lit 'a')) ^. plus (lit 'b');
    |]
    "aaabb";
  [%expect
    {|
    "abbc" -> rule 0, len 4, [x="bb"]
    "aaabb" -> rule 0, len 5, [x="aaa", y="bb"]
    "aaabbb" -> rule 0, len 6, [x="bbb"]
    "b" -> rule 0, len 1, [x="b"]
    "aaabb" -> rule 1, len 5, [x="aaa"]
    |}]

let%expect_test "greedy disambiguation" =
  oracle [| star (lit 'a') ^. capture "x" (plus (lit 'a')) |] "aaa";
  oracle [| capture "z" (rep (cls 'a' 'c') 0 2) ^. plus (lit 'a') |] "aaa";
  oracle [| star (lit 'a') ^. capture "x" (lit 'a' ^. plus (lit 'b')) |] "aabb";
  oracle
    [|
      star (lit 'a')
      ^. capture "y" (alt (lit 'a' ^. cls 'a' 'c') (star (cls 'a' 'c')));
    |]
    "aba";
  [%expect
    {|
    "aaa" -> rule 0, len 3, [x="a"]
    "aaa" -> rule 0, len 3, [z="aa"]
    "aabb" -> rule 0, len 4, [x="abb"]
    "aba" -> rule 0, len 3, [y="ba"]
    |}]

let%expect_test "left alternative vs earliest-longest" =
  (* sedlex takes the left alternative, ocamllex the longest match for the
     earlier sub-pattern; both are parses of the same lexeme. *)
  oracle
    [|
      alt (cls 'c' 'd') (lit 'd' ^. cls 'a' 'b')
      ^. capture "y" (star (cls 'b' 'd'));
    |]
    "db";
  oracle
    [|
      capture "x" (alt (star (lit 'b')) (cls 'c' 'd'))
      ^. alt (lit 'd' ^. lit 'd') (star (cls 'a' 'c'))
      ^. capture "z" (lit 'c' ^. lit 'd');
    |]
    "ccd";
  [%expect
    {|
    "db" -> rule 0, len 2, [y="b"] (ocamllex: rule 0, len 2, [y=""])
    "ccd" -> rule 0, len 3, [x="", z="cd"] (ocamllex: rule 0, len 3, [x="c", z="cd"])
    |}]

let%expect_test "ocamllex bug: loop before capture" =
  (* The only parse is y="c", w="b"; ocamllex 5.4.0 (the real tool, not just
     this simulation) reports y="", which no parse admits. *)
  oracle
    [|
      alt (lit 'a') (star (cls 'b' 'd'))
      ^. capture "y" (alt (lit 'c') (lit 'b' ^. lit 'b'))
      ^. star (rep (cls 'a' 'd') 2 2)
      ^. capture "w" (cls 'a' 'b');
    |]
    "ccdb";
  [%expect
    {| ERROR "ccdb" -> ref=rule 0, len 4, [w="b", y="c"] ocamllex=rule 0, len 4, [w="b", y=""] |}]

let%expect_test "qcheck: single rule" =
  qcheck (G.map2 (fun r s -> ([| r |], s)) (gen_ir ~eof:false) gen_input);
  [%expect {| |}]

let%expect_test "qcheck: two rules" =
  qcheck ~count:1000
    (G.map3
       (fun a b s -> ([| a; b |], s))
       (gen_ir ~eof:false) (gen_ir ~eof:false) gen_input);
  [%expect {| |}]
