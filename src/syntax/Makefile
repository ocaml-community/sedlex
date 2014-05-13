# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

SRCS=cset.mli cset.ml sedlex.mli sedlex.ml sedlex_ppx.ml
OCAMLC=ocamlfind ocamlc -package ppx_tools -w +A-4-9 -annot
OCAMLOPT=ocamlfind ocamlopt -package ppx_tools -w +A-4-9 -annot

all: ppx_sedlex$(EXE) sedlex.cma
opt: ppx_sedlex.opt$(EXE) sedlex.cmxa sedlex.cmxs

sedlex.cma: $(SRCS)
	$(OCAMLC) -a -o sedlex.cma $(SRCS)

sedlex.cmxa: $(SRCS)
	$(OCAMLOPT) -a -o sedlex.cmxa $(SRCS)

sedlex.cmxs: $(SRCS)
	$(OCAMLOPT) -shared -o sedlex.cmxs $(SRCS)

ppx_sedlex$(EXE): sedlex.cma
	$(OCAMLC) -o $@ -linkpkg -linkall sedlex.cma

ppx_sedlex.opt$(EXE): sedlex.cmxa
	$(OCAMLOPT) -o $@ -linkpkg -linkall sedlex.cmxa

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.annot ppx_sedlex$(EXE) ppx_sedlex.opt$(EXE)
