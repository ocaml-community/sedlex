open Sedlex_oracle
module G = QCheck2.Gen

(* ================================================================== *)
(* Hand-written tests                                                 *)
(* ================================================================== *)

let%expect_test "simple capture" =
  oracle
    [| (fun () -> lit 'a' ^. capture "x" (plus (lit 'b')) ^. lit 'c') |]
    "abbc";
  [%expect {| "abbc" -> rule 0, len 4, [x="bb"] |}]

let%expect_test "whole-match capture" =
  oracle [| (fun () -> capture "x" (plus (cls 'a' 'z'))) |] "hello";
  [%expect {| "hello" -> rule 0, len 5, [x="hello"] |}]

let%expect_test "two captures in sequence" =
  oracle
    [|
      (fun () -> capture "x" (plus (lit 'a')) ^. capture "y" (plus (lit 'b')));
    |]
    "aaabb";
  [%expect {| "aaabb" -> rule 0, len 5, [x="aaa", y="bb"] |}]

let%expect_test "variable-length capture" =
  oracle
    [| (fun () -> lit 'd' ^. capture "c" (star (cls 'a' 'c')) ^. lit 'd') |]
    "dabcd";
  [%expect {| "dabcd" -> rule 0, len 5, [c="abc"] |}]

let%expect_test "no match" =
  oracle
    [| (fun () -> lit 'a' ^. capture "x" (plus (lit 'b')) ^. lit 'c') |]
    "xyz";
  [%expect {| "xyz" -> no match |}]

let%expect_test "multi-rule" =
  oracle
    [|
      (fun () -> lit 'd' ^. capture "x" (plus (lit 'b')) ^. lit 'e');
      (fun () -> capture "x" (plus (cls 'a' 'c')));
    |]
    "dbbe";
  oracle
    [|
      (fun () -> lit 'd' ^. capture "x" (plus (lit 'b')) ^. lit 'e');
      (fun () -> capture "x" (plus (cls 'a' 'c')));
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
      (fun () -> capture "x" (plus (lit 'a')) ^. plus (lit 'b') ^. lit 'c');
      (fun () -> capture "x" (plus (lit 'a')) ^. plus (lit 'b'));
    |]
    "aaabb";
  [%expect {| "aaabb" -> rule 1, len 5, [x="aaa"] |}]

let%expect_test "bounded repetition" =
  oracle
    [| (fun () -> rep (lit 'a') 2 4 ^. capture "x" (plus (lit 'b'))) |]
    "aaabbb";
  oracle
    [| (fun () -> rep (lit 'a') 2 4 ^. capture "x" (plus (lit 'b'))) |]
    "abbb";
  [%expect
    {|
    "aaabbb" -> rule 0, len 6, [x="bbb"]
    "abbb" -> no match
    |}]

let%expect_test "complement" =
  (* Compl [a-c] matches any char NOT in a-c *)
  oracle
    [| (fun () -> lit 'd' ^. capture "x" (compl (cls 'a' 'c')) ^. lit 'd') |]
    "dxd";
  oracle
    [| (fun () -> lit 'd' ^. capture "x" (compl (cls 'a' 'c')) ^. lit 'd') |]
    "dad";
  [%expect {|
    "dxd" -> rule 0, len 3, [x="x"]
    "dad" -> no match
    |}]

let%expect_test "subtraction" =
  (* Sub([a-e], [c-e]) = [a-b] *)
  oracle
    [| (fun () -> capture "x" (plus (sub (cls 'a' 'e') (cls 'c' 'e')))) |]
    "abcde";
  [%expect {| "abcde" -> rule 0, len 2, [x="ab"] |}]

let%expect_test "intersection" =
  (* Inter([a-d], [c-f]) = [c-d] *)
  oracle
    [| (fun () -> capture "x" (plus (inter (cls 'a' 'd') (cls 'c' 'f')))) |]
    "cdabe";
  [%expect {| "cdabe" -> rule 0, len 2, [x="cd"] |}]

let%expect_test "or-pattern with discriminator" =
  (* (lit 'a' as x) | (lit 'b' as x) — each branch binds "x" with different tags *)
  oracle
    [| (fun () -> alt (capture "x" (lit 'a')) (capture "x" (lit 'b'))) |]
    "a";
  oracle
    [| (fun () -> alt (capture "x" (lit 'a')) (capture "x" (lit 'b'))) |]
    "b";
  oracle
    [| (fun () -> alt (capture "x" (lit 'a')) (capture "x" (lit 'b'))) |]
    "c";
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
      (fun () ->
        alt
          (capture "x" (plus (lit 'a')))
          (capture "x" (seq (lit 'b') (plus (lit 'c')))));
    |]
    "aaa";
  oracle
    [|
      (fun () ->
        alt
          (capture "x" (plus (lit 'a')))
          (capture "x" (seq (lit 'b') (plus (lit 'c')))));
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
     own path from recording its tag. See the FIXME on add_node. *)

  (* Disjoint char sets: works correctly *)
  oracle [| (fun () -> star (lit 'a') ^. capture "x" (lit 'b')) |] "aab";
  (* Overlapping char sets: capture is wrong (should be "a", gets "") *)
  oracle [| (fun () -> star (lit 'a') ^. capture "x" (lit 'a')) |] "a";
  oracle [| (fun () -> star (lit 'a') ^. capture "x" (lit 'a')) |] "aaa";
  (* Plus has the same issue *)
  oracle [| (fun () -> plus (lit 'a') ^. capture "x" (lit 'a')) |] "aa";
  [%expect
    {|
    "aab" -> rule 0, len 3, [x="b"]
    FIXME "a" -> nfa=rule 0, len 1, [x="a"] dfa=rule 0, len 1, [x=""]
    FIXME "aaa" -> nfa=rule 0, len 3, [x="a"] dfa=rule 0, len 3, [x=""]
    FIXME "aa" -> nfa=rule 0, len 2, [x="a"] dfa=rule 0, len 2, [x=""]
    |}]

let%expect_test "qcheck: single rule" =
  let gen = G.map2 (fun r s -> ([| r |], s)) gen_desc gen_input in
  QCheck2.Test.(
    check_exn
      (make ~count:1000 ~name:"single" ~print:print_case gen
         (qcheck_oracle ~check_tags:false)));
  [%expect {| |}]

let%expect_test "qcheck: two rules" =
  let gen = G.map3 (fun a b s -> ([| a; b |], s)) gen_desc gen_desc gen_input in
  QCheck2.Test.(
    check_exn
      (make ~count:1000 ~name:"two rules" ~print:print_case gen
         (qcheck_oracle ~check_tags:false)));
  [%expect {| |}]

let%expect_test "qcheck: single rule with tag verification" =
  let gen = G.map2 (fun r s -> ([| r |], s)) gen_desc gen_input in
  let test =
    QCheck2.Test.make ~count:1000 ~max_fail:10 ~name:"single+tags"
      ~print:(fun (descs, input_str) ->
        Array.iteri (fun i d -> Printf.printf "  rule%d: %s\n" i (show d)) descs;
        oracle (Array.map (fun d () -> d) descs) input_str;
        "")
      gen
      (qcheck_oracle ~check_tags:true)
  in
  (try QCheck2.Test.check_exn test with QCheck2.Test.Test_fail _ -> ());
  [%expect
    {|
      rule0: (((a | a) | (a as z)) | a)
    FIXME "a" -> nfa=rule 0, len 1, [] dfa=rule 0, len 1, [z="a"]
      rule0: ((a*, a*)*, ([a-c] as x))
    FIXME "a" -> nfa=rule 0, len 1, [x="a"] dfa=rule 0, len 1, [x=""]
      rule0: ((a | a*), ((a*, (([a-c], [a-c])*+++ as x)), ((a | (((a | a), a*) as x)), (a | a*))))
    FIXME "a" -> nfa=rule 0, len 1, [x="", x="a"] dfa=rule 0, len 1, [x=""]
      rule0: (((a* | (a as y)), (a | a*)) | a)
    FIXME "" -> nfa=rule 0, len 0, [y=<unset>] dfa=rule 0, len 0, [y=<unset>]
      rule0: (b*, (b as x))
    FIXME "b" -> nfa=rule 0, len 1, [x="b"] dfa=rule 0, len 1, [x=""]
      rule0: (((a | a*), (a* | a)), ((a | a) as z))
    FIXME "a" -> nfa=rule 0, len 1, [z="a"] dfa=rule 0, len 1, [z=""]
      rule0: ((a | (a*, ((a, (a, [a-c]))* | (a as y)))), c)
    FIXME "c" -> nfa=rule 0, len 1, [y=<unset>] dfa=rule 0, len 1, [y=<unset>]
      rule0: (((([a-c] | a), (a | (b, [a-c]+))) as x), c)
    FIXME "abac" -> nfa=rule 0, len 4, [x="aba"] dfa=rule 0, len 4, [x="abac"]
      rule0: ([a-c]*, (b+ as x))
    FIXME "b" -> nfa=rule 0, len 1, [x="b"] dfa=rule 0, len 1, [x=""]
      rule0: ([a-c]*, (a as x))
    FIXME "aa" -> nfa=rule 0, len 2, [x="a"] dfa=rule 0, len 2, [x=""]
    |}]
