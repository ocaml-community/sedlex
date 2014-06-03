# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

.PHONY: test
test: tokenizer$(EXE) complement$(EXE)
	./tokenizer$(EXE)
	./complement$(EXE)

tokenizer$(EXE): tokenizer.ml
	ocamlc -ppx "../src/syntax/ppx_sedlex$(EXE)" -I ../src/lib -o tokenizer$(EXE) sedlexing.cma tokenizer.ml

complement$(EXE): complement.ml
	ocamlc -ppx "../src/syntax/ppx_sedlex$(EXE)" -I ../src/lib -o complement$(EXE) sedlexing.cma complement.ml

.PHONY: with_driver
with_driver: tokenizer.ml
	ocamlc -ppx "ppx_driver ../src/syntax/sedlex.cma" -I ../src/lib -o tokenizer$(EXE) sedlexing.cma tokenizer.ml

.PHONY: with_findlib
with_findlib: tokenizer.ml
	ocamlfind ocamlc -c -package sedlex -ppx "ocamlfind sedlex/ppx_sedlex" tokenizer.ml
	ocamlfind ocamlc -o tokenizer$(EXE) -linkpkg -package sedlex tokenizer.cmo

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj tokenizer$(EXE) complement$(EXE)
