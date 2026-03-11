open Ppxlib
module P = Sedlex_ppx.Ppx_sedlex
module S = Sedlex_ppx.Sedlex

let reset_state () =
  P.partition_counter := 0;
  P.table_counter := 0;
  Hashtbl.clear P.partitions;
  Hashtbl.clear P.tables

let clear_tables () =
  Hashtbl.clear P.partitions;
  Hashtbl.clear P.tables

let expand_with ~shortest ~ctxt:_ expr =
  reset_state ();
  let loc = Location.none in
  let code_expr, auto =
    P.handle_sedlex_match ~shortest ~env:P.builtin_regexps ~map_rhs:Fun.id expr
  in
  let code_str = Pprintast.string_of_expression code_expr in
  let dot_str = S.dfa_to_dot auto in
  clear_tables ();
  [%expr
    print_string "DOT:\n";
    print_string [%e Ast_builder.Default.estring ~loc dot_str];
    print_string "CODE:\n";
    print_string [%e Ast_builder.Default.estring ~loc code_str];
    print_newline ()]

let ext =
  Extension.V3.declare "sedlex_test" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (expand_with ~shortest:false)

let ext_shortest =
  Extension.V3.declare "sedlex_test_shortest" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (expand_with ~shortest:true)

let () =
  Driver.register_transformation "sedlex_test" ~extensions:[ext; ext_shortest]
