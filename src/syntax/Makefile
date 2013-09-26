# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

SRCS=cset.mli cset.ml sedlex.mli sedlex.ml sedlex_ppx.ml
OCAMLC=ocamlfind ocamlc -package ppx_tools -w +A-4-9 -annot
OCAMLOPT=ocamlfind ocamlopt -package ppx_tools -w +A-4-9 -annot

all: sedlex.exe sedlex.cma
opt: sedlex.opt.exe sedlex.cmxa sedlex.cmxs

sedlex.cma: $(SRCS)
	$(OCAMLC) -a -o sedlex.cma $(SRCS)

sedlex.cmxa: $(SRCS)
	$(OCAMLOPT) -a -o sedlex.cmxa $(SRCS)

sedlex.cmxs: $(SRCS)
	$(OCAMLOPT) -shared -o sedlex.cmxs $(SRCS)

sedlex.exe: sedlex.cma
	$(OCAMLC) -o sedlex.exe -linkpkg -linkall sedlex.cma

sedlex.opt.exe: sedlex.cmxa
	$(OCAMLOPT) -o sedlex.opt.exe -linkpkg -linkall sedlex.cmxa

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
