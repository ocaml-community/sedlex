[@@@ocaml.ppx.context
{
  tool_name = "ppx_driver";
  include_dirs = [];
  load_path = [];
  open_modules = [];
  for_package = None;
  debug = false;
  use_threads = false;
  use_vmthreads = false;
  recursive_types = false;
  principal = false;
  transparent_modules = false;
  unboxed_types = false;
  unsafe_string = false;
  cookies =
    [
      ( "sedlex.regexps",
        [%regexps
          let digit_2 = [%sedlex.regexp? '0' .. '1']
          let digit_8 = [%sedlex.regexp? '0' .. '7']
          let digit = [%sedlex.regexp? '0' .. '9']
          let digit_16 = [%sedlex.regexp? digit | 'A' .. 'F' | 'a' .. 'f']
          let prefix_2 = [%sedlex.regexp? "0b"]
          let prefix_8 = [%sedlex.regexp? "0o"]
          let prefix_16 = [%sedlex.regexp? "0x"]
          let sign = [%sedlex.regexp? "" | '+' | '-']
          let sign_op = [%sedlex.regexp? '+' | '-']
          let num_2 = [%sedlex.regexp? Plus digit_2]
          let num_8 = [%sedlex.regexp? Plus digit_8]
          let num_10 = [%sedlex.regexp? Plus digit]
          let num_16 = [%sedlex.regexp? Plus digit_16]] );
    ];
}]

let __sedlex_tl = function _ :: tl -> tl | _ -> assert false

let __sedlex_table_6 =
  "\001\000\001\000\002\003\003\003\003\003\003\003\003\003\003"

let __sedlex_table_8 =
  "\001\001\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002"

let __sedlex_table_12 =
  "\001\001\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\001\001\001\001\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\001\001\001\001\001\000\000\002"

let __sedlex_table_2 =
  "\001\000\001\000\002\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\006"

let __sedlex_table_1 =
  "\001\001\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001"

let __sedlex_table_5 =
  "\001\001\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\001\001\001\001\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\001\001\001\001\001"

let __sedlex_table_7 = "\001\000\001\000\002\003\003\003\003\003\003\003\003"

let __sedlex_table_3 =
  "\001\000\001\000\002\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\000\000\003\003\003\003\003\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\003\003\003\003\003"

let __sedlex_table_13 =
  "\001\000\000\000\000\000\000\000\000\000\002\002\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\003\000\003\000\000\004\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006"

let __sedlex_table_10 =
  "\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002"

let __sedlex_table_4 = "\001\002\002\002\002\002\002\002\002\002"

let __sedlex_table_11 =
  "\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002"

let __sedlex_table_9 = "\001\000\001\000\002\003\003"

let __sedlex_partition_2 c =
  if c <= 8 then -1
  else if c <= 32 then
    Char.code (String.unsafe_get __sedlex_table_1 (c - 9)) - 1
  else -1

let __sedlex_partition_4 c =
  if c <= 42 then -1
  else if c <= 120 then
    Char.code (String.unsafe_get __sedlex_table_2 (c - 43)) - 1
  else -1

let __sedlex_partition_15 c =
  if c <= 42 then -1
  else if c <= 102 then
    Char.code (String.unsafe_get __sedlex_table_3 (c - 43)) - 1
  else -1

let __sedlex_partition_3 c =
  if c <= 47 then -1
  else if c <= 57 then
    Char.code (String.unsafe_get __sedlex_table_4 (c - 48)) - 1
  else -1

let __sedlex_partition_8 c = if c <= 47 then -1 else if c <= 49 then 0 else -1
let __sedlex_partition_5 c = if c <= 47 then -1 else if c <= 57 then 0 else -1

let __sedlex_partition_14 c =
  if c <= 47 then -1
  else if c <= 102 then
    Char.code (String.unsafe_get __sedlex_table_5 (c - 48)) - 1
  else -1

let __sedlex_partition_7 c =
  if c <= 42 then -1
  else if c <= 57 then
    Char.code (String.unsafe_get __sedlex_table_6 (c - 43)) - 1
  else -1

let __sedlex_partition_12 c =
  if c <= 42 then -1
  else if c <= 55 then
    Char.code (String.unsafe_get __sedlex_table_7 (c - 43)) - 1
  else -1

let __sedlex_partition_11 c = if c <= 47 then -1 else if c <= 55 then 0 else -1

let __sedlex_partition_6 c =
  if c <= 47 then -1
  else if c <= 105 then
    Char.code (String.unsafe_get __sedlex_table_8 (c - 48)) - 1
  else -1

let __sedlex_partition_9 c =
  if c <= 42 then -1
  else if c <= 49 then
    Char.code (String.unsafe_get __sedlex_table_9 (c - 43)) - 1
  else -1

let __sedlex_partition_10 c =
  if c <= 47 then -1
  else if c <= 105 then
    Char.code (String.unsafe_get __sedlex_table_10 (c - 48)) - 1
  else -1

let __sedlex_partition_13 c =
  if c <= 47 then -1
  else if c <= 105 then
    Char.code (String.unsafe_get __sedlex_table_11 (c - 48)) - 1
  else -1

let __sedlex_partition_16 c =
  if c <= 47 then -1
  else if c <= 105 then
    Char.code (String.unsafe_get __sedlex_table_12 (c - 48)) - 1
  else -1

let __sedlex_partition_1 c =
  if c <= 255 then Char.code (String.unsafe_get __sedlex_table_13 (c - -1)) - 1
  else -1

let rec token buf =
  let sub (a, b) = Sedlexing.Latin1.sub_lexeme buf a b in
  let rec __sedlex_state_0 __sedlex_path buf =
    match __sedlex_partition_1 (Sedlexing.__private__next_int buf) with
      | 0 -> (14, __sedlex_path)
      | 1 -> __sedlex_state_2 (2 :: __sedlex_path) buf
      | 2 -> __sedlex_state_3 (3 :: __sedlex_path) buf
      | 3 -> __sedlex_state_4 (4 :: __sedlex_path) buf
      | 4 -> __sedlex_state_10 (10 :: __sedlex_path) buf
      | 5 -> (13, __sedlex_path)
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_2 __sedlex_path buf =
    Sedlexing.mark buf 12;
    match __sedlex_partition_2 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_2 (2 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_3 __sedlex_path buf =
    match __sedlex_partition_3 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_4 (4 :: __sedlex_path) buf
      | 1 -> __sedlex_state_10 (10 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_4 __sedlex_path buf =
    Sedlexing.mark buf 2;
    match __sedlex_partition_4 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_5 (5 :: __sedlex_path) buf
      | 1 -> __sedlex_state_8 (8 :: __sedlex_path) buf
      | 2 -> __sedlex_state_10 (10 :: __sedlex_path) buf
      | 3 -> __sedlex_state_11 (11 :: __sedlex_path) buf
      | 4 -> __sedlex_state_18 (18 :: __sedlex_path) buf
      | 5 -> __sedlex_state_25 (25 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_5 __sedlex_path buf =
    match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_6 (6 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_6 __sedlex_path buf =
    match __sedlex_partition_6 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_6 (6 :: __sedlex_path) buf
      | 1 -> (10, __sedlex_path)
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_8 __sedlex_path buf =
    match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 (9 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_9 __sedlex_path buf =
    Sedlexing.mark buf 6;
    match __sedlex_partition_5 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_9 (9 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_10 __sedlex_path buf =
    Sedlexing.mark buf 2;
    match __sedlex_partition_7 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_5 (5 :: __sedlex_path) buf
      | 1 -> __sedlex_state_8 (8 :: __sedlex_path) buf
      | 2 -> __sedlex_state_10 (10 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_11 __sedlex_path buf =
    match __sedlex_partition_8 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_12 (12 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_12 __sedlex_path buf =
    Sedlexing.mark buf 0;
    match __sedlex_partition_9 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_13 (13 :: __sedlex_path) buf
      | 1 -> __sedlex_state_16 (16 :: __sedlex_path) buf
      | 2 -> __sedlex_state_12 (12 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_13 __sedlex_path buf =
    match __sedlex_partition_8 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_14 (14 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_14 __sedlex_path buf =
    match __sedlex_partition_10 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_14 (14 :: __sedlex_path) buf
      | 1 -> (8, __sedlex_path)
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_16 __sedlex_path buf =
    match __sedlex_partition_8 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_17 (17 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_17 __sedlex_path buf =
    Sedlexing.mark buf 4;
    match __sedlex_partition_8 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_17 (17 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_18 __sedlex_path buf =
    match __sedlex_partition_11 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_19 (19 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_19 __sedlex_path buf =
    Sedlexing.mark buf 1;
    match __sedlex_partition_12 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_20 (20 :: __sedlex_path) buf
      | 1 -> __sedlex_state_23 (23 :: __sedlex_path) buf
      | 2 -> __sedlex_state_19 (19 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_20 __sedlex_path buf =
    match __sedlex_partition_11 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_21 (21 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_21 __sedlex_path buf =
    match __sedlex_partition_13 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_21 (21 :: __sedlex_path) buf
      | 1 -> (9, __sedlex_path)
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_23 __sedlex_path buf =
    match __sedlex_partition_11 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_24 (24 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_24 __sedlex_path buf =
    Sedlexing.mark buf 5;
    match __sedlex_partition_11 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_24 (24 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_25 __sedlex_path buf =
    match __sedlex_partition_14 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_26 (26 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_26 __sedlex_path buf =
    Sedlexing.mark buf 3;
    match __sedlex_partition_15 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_27 (27 :: __sedlex_path) buf
      | 1 -> __sedlex_state_30 (30 :: __sedlex_path) buf
      | 2 -> __sedlex_state_26 (26 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_27 __sedlex_path buf =
    match __sedlex_partition_14 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_28 (28 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_28 __sedlex_path buf =
    match __sedlex_partition_16 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_28 (28 :: __sedlex_path) buf
      | 1 -> (11, __sedlex_path)
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_30 __sedlex_path buf =
    match __sedlex_partition_14 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_31 (31 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_state_31 __sedlex_path buf =
    Sedlexing.mark buf 7;
    match __sedlex_partition_14 (Sedlexing.__private__next_int buf) with
      | 0 -> __sedlex_state_31 (31 :: __sedlex_path) buf
      | _ -> (Sedlexing.backtrack buf, __sedlex_tl __sedlex_path)
  and __sedlex_trace_0 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 12 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 11 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 10, 11 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 7, 11 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 12 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 10, 12 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 11 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 10 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_1 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 12 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 11 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 7, 4 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 10, 18 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 10, 19 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 18 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 19 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 11 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 10 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_2 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 10 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 9 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 5 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 4 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 10 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 10 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 3 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 4 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 0 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 8 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_3 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 12 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 11 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 10, 25 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 10, 26 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 7, 25 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 11 __sedlex_rest
            | 7, 26 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 10 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_4 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 17 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 16 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 12, 12 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | 10, 11 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 7, 11 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 12 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 15, 17 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 12, 17 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 10, 12 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 16 __sedlex_rest
            | 15, 16 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 12, 16 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 15 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_5 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 17 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 16 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 12, 23 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 10, 18 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 12, 24 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 15, 23 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 10, 19 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 18 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 19 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 15, 24 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 16 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 12, 19 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 15 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_6 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 15 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 14 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 5 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 13, 9 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | 10, 9 -> __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | 10, 8 -> __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | 5, 4 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 10, 4 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 8 __sedlex_rest
            | 5, 10 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 10 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 3 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 4 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 8, 0 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 13, 8 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | 10, 10 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 8 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 13 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_7 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 17 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 16 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 15, 30 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 10, 25 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 12, 31 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 10, 26 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 12, 26 ->
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 10 __sedlex_rest
            | 12, 30 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 7, 25 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 15, 31 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 16 __sedlex_rest
            | 7, 26 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 15 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_8 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 20 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 19 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 14, 13 -> __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 11, 11 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 17, 13 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 18, 14 -> __sedlex_aux (__sedlex_pos - 1) 17 __sedlex_rest
            | 11, 12 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 11 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 12 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 14, 14 -> __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 17, 14 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 19 __sedlex_rest
            | 14, 12 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 11 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 18 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_9 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 20 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 19 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 17, 20 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 14, 20 -> __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 11, 19 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 14, 19 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 11 __sedlex_rest
            | 7, 18 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 18, 21 -> __sedlex_aux (__sedlex_pos - 1) 17 __sedlex_rest
            | 11, 18 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 7, 19 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 19 __sedlex_rest
            | 17, 21 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 14, 21 -> __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 18 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_10 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 18 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 17 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 5 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 12, 6 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 12, 10 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 9 __sedlex_rest
            | 9, 3 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 15, 5 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 5, 4 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 12, 4 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 9 __sedlex_rest
            | 16, 6 -> __sedlex_aux (__sedlex_pos - 1) 15 __sedlex_rest
            | 5, 10 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 12, 5 -> __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | 9, 0 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 9, 10 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 9, 4 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 15, 6 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 12 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 16 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  and __sedlex_trace_11 buf __sedlex_path =
    let __sedlex_aliases_pos = [| 0; 0; 0; 0 |]
    and __sedlex_aliases_len = [| 0; 0; 0; 0 |] in
    let rec __sedlex_aux __sedlex_pos __sedlex_curr = function
      | [] -> (
          match __sedlex_curr with
            | 20 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 19 ->
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | 4 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aliases_len.(3) <-
                  __sedlex_aliases_pos.(3) - __sedlex_pos;
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                ()
            | _ -> assert false)
      | __sedlex_state :: __sedlex_rest -> (
          match (__sedlex_curr, __sedlex_state) with
            | 18, 28 -> __sedlex_aux (__sedlex_pos - 1) 17 __sedlex_rest
            | 7, 4 ->
                __sedlex_aliases_len.(2) <-
                  __sedlex_aliases_pos.(2) - __sedlex_pos;
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 5 __sedlex_rest
            | 5, 0 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 17, 28 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 14, 26 ->
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aliases_len.(0) <-
                  __sedlex_aliases_pos.(0) - __sedlex_pos;
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 11 __sedlex_rest
            | 7, 25 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 14, 27 -> __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 11, 25 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 4, 0 ->
                __sedlex_aliases_pos.(3) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 19 __sedlex_rest
            | 7, 26 -> __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | 17, 27 ->
                __sedlex_aliases_pos.(0) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 14, 28 -> __sedlex_aux (__sedlex_pos - 1) 14 __sedlex_rest
            | 5, 3 -> __sedlex_aux (__sedlex_pos - 1) 4 __sedlex_rest
            | 11, 26 ->
                __sedlex_aliases_pos.(2) <- __sedlex_pos;
                __sedlex_aliases_len.(1) <-
                  __sedlex_aliases_pos.(1) - __sedlex_pos;
                __sedlex_aliases_pos.(1) <- __sedlex_pos;
                __sedlex_aux (__sedlex_pos - 1) 7 __sedlex_rest
            | _ -> assert false)
    in
    __sedlex_aux (Sedlexing.lexeme_length buf) 18 __sedlex_path;
    (__sedlex_aliases_pos, __sedlex_aliases_len)
  in
  Sedlexing.start buf;
  let __sedlex_result, __sedlex_path = __sedlex_state_0 [0] buf in
  match __sedlex_result with
    | 0 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_0 buf __sedlex_path
        in
        let n = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and s = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1)) in
        Printf.printf "Bin %s%s\n" (sub s) (sub n);
        token buf
    | 1 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_1 buf __sedlex_path
        in
        let n = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and s = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1)) in
        Printf.printf "Oct %s%s\n" (sub s) (sub n);
        token buf
    | 2 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_2 buf __sedlex_path
        in
        let n = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and s = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1)) in
        Printf.printf "Dec %s%s\n" (sub s) (sub n);
        token buf
    | 3 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_3 buf __sedlex_path
        in
        let n = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and s = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1)) in
        Printf.printf "Hex %s%s\n" (sub s) (sub n);
        token buf
    | 4 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_4 buf __sedlex_path
        in
        let d = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and n = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and s = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2)) in
        Printf.printf "Bin %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | 5 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_5 buf __sedlex_path
        in
        let d = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and n = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and s = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2)) in
        Printf.printf "Oct %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | 6 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_6 buf __sedlex_path
        in
        let d = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and n = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and s = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2)) in
        Printf.printf "Dec %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | 7 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_7 buf __sedlex_path
        in
        let d = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and n = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and s = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2)) in
        Printf.printf "Hex %s%s/%s\n" (sub s) (sub n) (sub d);
        token buf
    | 8 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_8 buf __sedlex_path
        in
        let i = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and o = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and r = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2))
        and s = (__sedlex_aliases_pos.(3), __sedlex_aliases_len.(3)) in
        Printf.printf "Bin %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | 9 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_9 buf __sedlex_path
        in
        let i = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and o = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and r = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2))
        and s = (__sedlex_aliases_pos.(3), __sedlex_aliases_len.(3)) in
        Printf.printf "Oct %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | 10 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_10 buf __sedlex_path
        in
        let i = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and o = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and r = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2))
        and s = (__sedlex_aliases_pos.(3), __sedlex_aliases_len.(3)) in
        Printf.printf "Dec %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | 11 ->
        let __sedlex_aliases_pos, __sedlex_aliases_len =
          __sedlex_trace_11 buf __sedlex_path
        in
        let i = (__sedlex_aliases_pos.(0), __sedlex_aliases_len.(0))
        and o = (__sedlex_aliases_pos.(1), __sedlex_aliases_len.(1))
        and r = (__sedlex_aliases_pos.(2), __sedlex_aliases_len.(2))
        and s = (__sedlex_aliases_pos.(3), __sedlex_aliases_len.(3)) in
        Printf.printf "Hex %s%s%s%si\n" (sub s) (sub r) (sub o) (sub i);
        token buf
    | 12 -> token buf
    | 13 -> print_endline "Non ASCII"
    | 14 -> print_endline "EOF"
    | _ -> failwith "Unexpected character"

let () =
  let lexbuf =
    Sedlexing.Latin1.from_string
      {|
        123
        +123
        -123
        0b01010101
        -0b11110000
        +0b11111111
        0o12345670
        +0o76543210
        -0o17263540
        0x123abcdef
        -0x456DEFabc
        +0x789ABCdef
        123/456
        -456/789
        +987/654
        0o777/100
        +0o200/666
        1+1i
        1-1i
        0x1f+2ei
        +0x1f-2ei
        0b10+11i
        -0b10-11i
      |}
  in
  token lexbuf
