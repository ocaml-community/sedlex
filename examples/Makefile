# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

tokenizer.exe: tokenizer.ml
	ocamlc -ppx "../src/syntax/sedlex.exe" -I ../src/lib -o tokenizer.exe sedlexing.cma tokenizer.ml

.PHONY: with_driver
with_driver: tokenizer.ml
	ocamlc -ppx "ppx_driver ../src/syntax/sedlex.cma" -I ../src/lib -o tokenizer.exe sedlexing.cma tokenizer.ml

.PHONY: with_findlib
with_findlib: tokenizer.ml
	ocamlfind ocamlc -c -package sedlex -ppx "ocamlfind sedlex/sedlex.exe" tokenizer.ml
	ocamlfind ocamlc -o tokenizer.exe -linkpkg -package sedlex tokenizer.cmo

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
