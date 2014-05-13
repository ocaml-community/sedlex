# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

SRCS=cset.mli cset.ml sedlex.mli sedlex.ml sedlex_ppx.ml
OCAMLC=ocamlfind ocamlc -package ppx_tools -w +A-4-9 -annot
OCAMLOPT=ocamlfind ocamlopt -package ppx_tools -w +A-4-9 -annot

all: sedlex$(EXE) sedlex.cma
opt: sedlex.opt$(EXE) sedlex.cmxa sedlex.cmxs

sedlex.cma: $(SRCS)
	$(OCAMLC) -a -o sedlex.cma $(SRCS)

sedlex.cmxa: $(SRCS)
	$(OCAMLOPT) -a -o sedlex.cmxa $(SRCS)

sedlex.cmxs: $(SRCS)
	$(OCAMLOPT) -shared -o sedlex.cmxs $(SRCS)

sedlex$(EXE): sedlex.cma
	$(OCAMLC) -o sedlex$(EXE) -linkpkg -linkall sedlex.cma

sedlex.opt$(EXE): sedlex.cmxa
	$(OCAMLOPT) -o sedlex.opt$(EXE) -linkpkg -linkall sedlex.cmxa

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.annot sedlex$(EXE) sedlex.opt$(EXE)
