open Ppxlib
module P = Sedlex_ppx.Ppx_sedlex
module S = Sedlex_compiler.Sedlex

let expand ~ctxt:_ expr =
  P.reset_state ();
  S.reset_tags ();
  let loc = Location.none in
  let code_expr, auto = P.handle_sedlex_match expr in
  let code_str = Pprintast.string_of_expression code_expr in
  let dot_str = S.dfa_to_dot auto in
  P.reset_state ();
  [%expr
    print_string "DOT:\n";
    print_string [%e Ast_builder.Default.estring ~loc dot_str];
    print_string "CODE:\n";
    print_string [%e Ast_builder.Default.estring ~loc code_str];
    print_newline ()]

let strip_line_numbers s =
  let s = Str.global_replace (Str.regexp "line [0-9]+, ") "" s in
  let blank_digits m =
    let t = Str.matched_string m in
    String.init (String.length t) (fun i -> if t.[i] = '|' then '|' else ' ')
  in
  Str.global_substitute (Str.regexp "^ *[0-9]+ |") blank_digits s

let expand_error ~ctxt:_ expr =
  P.reset_state ();
  S.reset_tags ();
  let loc = Location.none in
  let msg =
    try
      let _ = P.map_expression expr in
      "NO ERROR"
    with exn ->
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      Location.report_exception fmt exn;
      Format.pp_print_flush fmt ();
      strip_line_numbers (Buffer.contents buf)
  in
  P.reset_state ();
  [%expr
    print_string [%e Ast_builder.Default.estring ~loc msg];
    print_newline ()]

let ext =
  Extension.V3.declare "sedlex_test" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let ext_error =
  Extension.V3.declare "compile_error" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_error

let () =
  Driver.register_transformation "sedlex_test" ~extensions:[ext; ext_error]
