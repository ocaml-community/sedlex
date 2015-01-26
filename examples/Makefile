# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

.PHONY: test
test: tokenizer$(EXE) complement$(EXE) substraction$(EXE)
	./tokenizer$(EXE)
	./complement$(EXE)
	./substraction$(EXE)

tokenizer$(EXE): tokenizer.ml
	ocamlfind ocamlc -package gen -ppx "../src/syntax/ppx_sedlex$(EXE)" -I ../src/lib -o tokenizer$(EXE) -linkpkg sedlexing.cma tokenizer.ml

complement$(EXE): complement.ml
	ocamlfind ocamlc -package gen -ppx "../src/syntax/ppx_sedlex$(EXE)" -I ../src/lib -o complement$(EXE) -linkpkg sedlexing.cma complement.ml

substraction$(EXE): substraction.ml
	ocamlfind ocamlc -package gen -ppx "../src/syntax/ppx_sedlex$(EXE)" -I ../src/lib -o substraction$(EXE) -linkpkg sedlexing.cma substraction.ml

.PHONY: with_driver
with_driver: tokenizer.ml
	ocamlfind ocamlc -package gen -ppx "ppx_driver ../src/syntax/sedlex.cma" -I ../src/lib -o tokenizer$(EXE) sedlexing.cma tokenizer.ml

.PHONY: with_findlib
with_findlib: tokenizer.ml
	ocamlfind ocamlc -c -package sedlex -package gen tokenizer.ml
	ocamlfind ocamlc -o tokenizer$(EXE) -linkpkg -package sedlex -package gen tokenizer.cmo

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj tokenizer$(EXE) complement$(EXE) substraction$(EXE)
