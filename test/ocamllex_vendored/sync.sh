#!/bin/sh
# Synchronize vendored ocamllex sources from the current OCaml compiler.
#
# Usage: ./test/ocamllex_vendored/sync.sh
#
# The source directory is the lex/ subtree of the OCaml compiler sources
# kept by opam.  Override with:  OCAML_LEX_DIR=/path/to/lex ./sync.sh

set -eu

DEST="$(cd "$(dirname "$0")" && pwd)"

if [ -z "${OCAML_LEX_DIR:-}" ]; then
  PREFIX="$(opam var prefix)"
  OCAML_VERSION="$(opam var ocaml:version)"
  OCAML_LEX_DIR="$PREFIX/.opam-switch/sources/ocaml-compiler.$OCAML_VERSION/lex"
fi

if [ ! -d "$OCAML_LEX_DIR" ]; then
  echo "error: source directory not found: $OCAML_LEX_DIR" >&2
  echo "Set OCAML_LEX_DIR to the lex/ directory of the OCaml compiler sources." >&2
  exit 1
fi

FILES="cset.ml cset.mli lexgen.ml lexgen.mli syntax.ml syntax.mli table.ml"

for f in $FILES; do
  cp "$OCAML_LEX_DIR/$f" "$DEST/$f"
done

echo "Synced from $OCAML_LEX_DIR (OCaml ${OCAML_VERSION:-unknown})"
