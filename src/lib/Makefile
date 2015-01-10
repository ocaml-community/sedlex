# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

MODS=sedlexing.cmo

all: sedlexing.cma
opt: sedlexing.cmxa

sedlexing.cma: $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlfind ocamlc -package gen -w +A-4-9 -safe-string -c $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlfind ocamlc -package gen -a -o sedlexing.cma $(MODS)

sedlexing.cmxa: $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlfind ocamlopt -package gen -safe-string -c $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlfind ocamlopt -package gen -a -o sedlexing.cmxa $(MODS:.cmo=.cmx)

doc:
	rm -rf ../../libdoc
	mkdir ../../libdoc
	ocamlfind ocamldoc -package gen -html sedlexing.mli -d ../../libdoc



clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
