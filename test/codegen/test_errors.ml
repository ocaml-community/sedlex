(* Error tests for `as` bindings *)

let%expect_test "error: as inside Star" =
  [%compile_error
    [%sedlex match buf with Star ('a' as x) -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 33-43:
      |     [%sedlex match buf with Star ('a' as x) -> ignore x | _ -> ()]];
                                         ^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Star
    |}]

let%expect_test "error: as inside Plus" =
  [%compile_error
    [%sedlex match buf with Plus ('a' as x) -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 33-43:
       |     [%sedlex match buf with Plus ('a' as x) -> ignore x | _ -> ()]];
                                          ^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Plus
    |}]

let%expect_test "error: as inside Opt" =
  [%compile_error
    [%sedlex match buf with Opt ('a' as x) -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 32-42:
       |     [%sedlex match buf with Opt ('a' as x) -> ignore x | _ -> ()]];
                                         ^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Opt
    |}]

let%expect_test "error: as inside Rep" =
  [%compile_error
    [%sedlex
      match buf with Rep ((('a' as x), 'b'), 2 .. 3) -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 26-43:
       |       match buf with Rep ((('a' as x), 'b'), 2 .. 3) -> ignore x | _ -> ()]];
                                   ^^^^^^^^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Rep
    |}]

let%expect_test "error: as inside Compl" =
  [%compile_error
    [%sedlex match buf with Compl ('a' as x) -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 34-44:
       |     [%sedlex match buf with Compl ('a' as x) -> ignore x | _ -> ()]];
                                           ^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Compl
    |}]

let%expect_test "error: as inside Sub" =
  [%compile_error
    [%sedlex match buf with Sub (('a' as x), 'b') -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 33-43:
       |     [%sedlex match buf with Sub (('a' as x), 'b') -> ignore x | _ -> ()]];
                                          ^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Sub
    |}]

let%expect_test "error: as inside Intersect" =
  [%compile_error
    [%sedlex match buf with Intersect (('a' as x), 'b') -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 39-49:
       |     [%sedlex match buf with Intersect (('a' as x), 'b') -> ignore x | _ -> ()]];
                                                ^^^^^^^^^^
    Error: Sedlex: 'as' bindings are not supported inside Intersect
    |}]

let%expect_test "error: as shadows inner binding" =
  [%compile_error
    [%sedlex match buf with (('a' as x), 'b') as x -> ignore x | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 49-50:
       |     [%sedlex match buf with (('a' as x), 'b') as x -> ignore x | _ -> ()]];
                                                          ^
    Error: Sedlex: 'as' binding 'x' shadows an inner binding of the same name
    |}]

let%expect_test "error: different names in or-pattern" =
  [%compile_error
    [%sedlex
      match buf with ('a' as x) | ('b' as y) -> ignore (x, y) | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 21-44:
       |       match buf with ('a' as x) | ('b' as y) -> ignore (x, y) | _ -> ()]];
                              ^^^^^^^^^^^^^^^^^^^^^^^
    Error: Sedlex: all branches of '|' must bind the same names with 'as'
    |}]

(* Error tests for Sub/Intersect/Compl on multi-char regexps *)

let%expect_test "error: Sub on multi-char regexp" =
  [%compile_error [%sedlex match buf with Sub ("ab", 'a') -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-57:
        |   [%compile_error [%sedlex match buf with Sub ("ab", 'a') -> () | _ -> ()]];
                                                    ^^^^^^^^^^^^^^^
    Error: Sedlex: the Sub operator can only applied to single-character length regexps
    |}]

let%expect_test "error: Intersect on multi-char regexp" =
  [%compile_error
    [%sedlex match buf with Intersect ("ab", 'a') -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 28-49:
        |     [%sedlex match buf with Intersect ("ab", 'a') -> () | _ -> ()]];
                                      ^^^^^^^^^^^^^^^^^^^^^
    Error: Sedlex: the Intersect operator can only applied to single-character length regexps
    |}]

let%expect_test "error: Compl on multi-char regexp" =
  [%compile_error [%sedlex match buf with Compl "ab" -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-52:
        |   [%compile_error [%sedlex match buf with Compl "ab" -> () | _ -> ()]];
                                                    ^^^^^^^^^^
    Error: Sedlex: the Compl operator can only applied to a single-character length regexp
    |}]

let%expect_test "error: Sub with one argument" =
  [%compile_error [%sedlex match buf with Sub 'a' -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-49:
        |   [%compile_error [%sedlex match buf with Sub 'a' -> () | _ -> ()]];
                                                    ^^^^^^^
    Error: Sedlex: the Sub operator requires two arguments, like Sub(a,b)
    |}]

let%expect_test "error: Intersect with one argument" =
  [%compile_error [%sedlex match buf with Intersect 'a' -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-55:
        |   [%compile_error [%sedlex match buf with Intersect 'a' -> () | _ -> ()]];
                                                    ^^^^^^^^^^^^^
    Error: Sedlex: the Intersect operator requires two arguments, like Intersect(a,b)
    |}]

(* Error tests for Rep *)

let%expect_test "error: Rep invalid range" =
  [%compile_error [%sedlex match buf with Rep ('a', 5 .. 2) -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-59:
        |   [%compile_error [%sedlex match buf with Rep ('a', 5 .. 2) -> () | _ -> ()]];
                                                    ^^^^^^^^^^^^^^^^^
    Error: Sedlex: Invalid range for Rep operator
    |}]

let%expect_test "error: Rep with non-integer" =
  [%compile_error
    [%sedlex match buf with Rep ('a', 'b' .. 'c') -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 28-49:
        |     [%sedlex match buf with Rep ('a', 'b' .. 'c') -> () | _ -> ()]];
                                      ^^^^^^^^^^^^^^^^^^^^^
    Error: Sedlex: Rep must take an integer constant or interval
    |}]

let%expect_test "error: Rep with one argument" =
  [%compile_error [%sedlex match buf with Rep 'a' -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-49:
        |   [%compile_error [%sedlex match buf with Rep 'a' -> () | _ -> ()]];
                                                    ^^^^^^^
    Error: Sedlex: the Rep operator takes 2 arguments
    |}]

(* Error tests for Compl/Chars *)

let%expect_test "error: Compl without argument" =
  [%compile_error [%sedlex match buf with Compl -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-47:
        |   [%compile_error [%sedlex match buf with Compl -> () | _ -> ()]];
                                                    ^^^^^
    Error: Sedlex: the Compl operator requires an argument
    |}]

let%expect_test "error: Chars with non-string" =
  [%compile_error [%sedlex match buf with Chars 42 -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-50:
        |   [%compile_error [%sedlex match buf with Chars 42 -> () | _ -> ()]];
                                                    ^^^^^^^^
    Error: Sedlex: the Chars operator requires a string argument
    |}]

(* Error tests for unbound regexp and invalid patterns *)

let%expect_test "error: unbound regexp" =
  [%compile_error [%sedlex match buf with nonexistent_regexp -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-60:
        |   [%compile_error [%sedlex match buf with nonexistent_regexp -> () | _ -> ()]];
                                                    ^^^^^^^^^^^^^^^^^^
    Error: Sedlex: unbound regexp nonexistent_regexp
    |}]

let%expect_test "error: invalid pattern" =
  [%compile_error [%sedlex match buf with Some 'a' -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-50:
        |   [%compile_error [%sedlex match buf with Some 'a' -> () | _ -> ()]];
                                                    ^^^^^^^^
    Error: Sedlex: unknown sedlex operator Some
    |}]

let%expect_test "error: invalid interval type" =
  [%compile_error [%sedlex match buf with 1.0 .. 2.0 -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-52:
        |   [%compile_error [%sedlex match buf with 1.0 .. 2.0 -> () | _ -> ()]];
                                                    ^^^^^^^^^^
    Error: Sedlex: this pattern is not a valid interval regexp
    |}]

let%expect_test "error: invalid constant pattern" =
  [%compile_error [%sedlex match buf with 1.0 -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-45:
        |   [%compile_error [%sedlex match buf with 1.0 -> () | _ -> ()]];
                                                    ^^^
    Error: Sedlex: this pattern is not a valid regexp
    |}]

(* Error tests for match structure *)

let%expect_test "error: missing catch-all" =
  [%compile_error [%sedlex match buf with 'a' -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-45:
        |   [%compile_error [%sedlex match buf with 'a' -> ()]];
                                                    ^^^
    Error: Sedlex: the last branch must be a catch-all error case
    |}]

let%expect_test "error: when guard" =
  [%compile_error [%sedlex match buf with 'a' when true -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 51-55:
        |   [%compile_error [%sedlex match buf with 'a' when true -> () | _ -> ()]];
                                                             ^^^^
    Error: Sedlex: 'when' guards are not supported
    |}]

let%expect_test "error: matched expression must be a single identifier" =
  [%compile_error [%sedlex match foo bar with 'a' -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 33-40:
        |   [%compile_error [%sedlex match foo bar with 'a' -> () | _ -> ()]];
                                           ^^^^^^^
    Error: Sedlex: the matched expression must be a single identifier
    |}]

(* Error tests for malformed strings *)

let%expect_test "error: malformed ASCII string" =
  [%compile_error [%sedlex match buf with "\x80" -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-48:
        |   [%compile_error [%sedlex match buf with "\x80" -> () | _ -> ()]];
                                                    ^^^^^^
    Error: Sedlex: Malformed ASCII string
    |}]

let%expect_test "error: malformed UTF-8 string" =
  [%compile_error [%sedlex match buf with Utf8 "\x80" -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 47-53:
        |   [%compile_error [%sedlex match buf with Utf8 "\x80" -> () | _ -> ()]];
                                                         ^^^^^^
    Error: Sedlex: Malformed UTF-8 string
    |}]

let%expect_test "error: non-ASCII char interval" =
  [%compile_error [%sedlex match buf with '\x80' .. '\xff' -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-58:
        |   [%compile_error [%sedlex match buf with '\x80' .. '\xff' -> () | _ -> ()]];
                                                    ^^^^^^^^^^^^^^^^
    Error: Sedlex: this pattern is not a valid ASCII interval regexp
    |}]

(* Error tests for sedlex extension misuse *)

let%expect_test "error: sedlex not on match expression" =
  [%compile_error [%sedlex 42]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 18-30:
        |   [%compile_error [%sedlex 42]];
                            ^^^^^^^^^^^^
    Error: Sedlex: the %sedlex extension is only recognized on match expressions
    |}]

(* Error tests for bare operators (missing argument) *)

let%expect_test "error: Star without argument" =
  [%compile_error [%sedlex match buf with Star -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-46:
        |   [%compile_error [%sedlex match buf with Star -> () | _ -> ()]];
                                                    ^^^^
    Error: Sedlex: the Star operator requires an argument
    |}]

let%expect_test "error: Plus without argument" =
  [%compile_error [%sedlex match buf with Plus -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-46:
        |   [%compile_error [%sedlex match buf with Plus -> () | _ -> ()]];
                                                    ^^^^
    Error: Sedlex: the Plus operator requires an argument
    |}]

let%expect_test "error: Opt without argument" =
  [%compile_error [%sedlex match buf with Opt -> () | _ -> ()]];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 42-45:
        |   [%compile_error [%sedlex match buf with Opt -> () | _ -> ()]];
                                                    ^^^
    Error: Sedlex: the Opt operator requires an argument
    |}]

(* Error tests for regexp definitions *)

let%expect_test "error: as in regexp definition" =
  [%compile_error
    let dummy = [%sedlex.regexp? 'a' as x] in
    ignore dummy];
  [%expect
    {|
    File "test/codegen/test_errors.ml", characters 33-41:
        |     let dummy = [%sedlex.regexp? 'a' as x] in
                                           ^^^^^^^^
    Error: Sedlex: 'as' bindings are not allowed in regexp definitions
    |}]
