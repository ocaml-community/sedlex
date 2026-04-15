# Vendored ocamllex sources

These files are copied verbatim from the OCaml compiler's `lex/` directory.
They provide ocamllex's DFA compiler (`Lexgen.make_dfa`) which is used as an
independent reference oracle to test sedlex's tagged DFA compilation.

## Files

| File | Description |
|------|-------------|
| `cset.ml` / `.mli` | Character sets |
| `lexgen.ml` / `.mli` | DFA compiler with tagged transitions |
| `syntax.ml` / `.mli` | Regular expression AST |
| `table.ml` | DFA table representation |

## Updating

Run `./sync.sh` to copy the latest sources from the current opam switch:

```
./test/ocamllex_vendored/sync.sh
```

To sync from a specific directory:

```
OCAML_LEX_DIR=/path/to/ocaml/lex ./test/ocamllex_vendored/sync.sh
```

## License

These files are part of the OCaml compiler and are distributed under the
GNU Lesser General Public License version 2.1 (see file headers).
