open Ppxlib
module P = Sedlex_ppx.Ppx_sedlex
module S = Sedlex_ppx.Sedlex

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

let ext =
  Extension.V3.declare "sedlex_test" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let () = Driver.register_transformation "sedlex_test" ~extensions:[ext]
