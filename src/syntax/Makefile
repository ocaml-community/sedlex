# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

SRCS=sedlex_cset.mli sedlex_cset.ml unicode63.mli unicode63.ml sedlex.mli sedlex.ml ppx_sedlex.ml

OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
PACKAGES=-package ppx_tools_versioned.metaquot_405 -package ocaml-migrate-parsetree -package ppx_tools_versioned
OCAMLFLAGS=$(PACKAGES) -linkall -w +A-4-9-42 -annot

all: ppx_sedlex$(EXE) sedlex.cma
opt: ppx_sedlex.opt$(EXE) sedlex.cmxa sedlex.cmxs

sedlex.cma: $(SRCS)
	$(OCAMLC) $(OCAMLFLAGS) -a -o sedlex.cma $(SRCS)

sedlex.cmxa: $(SRCS)
	$(OCAMLOPT) $(OCAMLFLAGS) -a -o sedlex.cmxa $(SRCS)

sedlex.cmxs: $(SRCS)
	$(OCAMLOPT) $(OCAMLFLAGS) -shared -o sedlex.cmxs $(SRCS)

# We need the ocaml-migrate-parsetree.driver-main.cm{,x}a library to come last
# on the command-line when linking, since its initialization code must run
# *after* the one of sedlex.cm{,x}a. To ensure that this is the case, we
# manually pass its file to ocaml{c,opt}, as normally ocamlfind links in all the
# -package libraries *before* user code.

MIGRATE_PARSETREE_MAIN_BYTE= \
  $(shell ocamlfind query -predicates byte -a-format ocaml-migrate-parsetree.driver-main)
MIGRATE_PARSETREE_MAIN_NATIVE= \
  $(shell ocamlfind query -predicates native -a-format ocaml-migrate-parsetree.driver-main)

ppx_sedlex$(EXE): sedlex.cma
	$(OCAMLC) \
		-predicates ppx_driver \
		-o $@ -linkpkg -linkall \
		-package ppx_tools_versioned \
		$< \
		$(MIGRATE_PARSETREE_MAIN_BYTE)

ppx_sedlex.opt$(EXE): sedlex.cmxa
	$(OCAMLOPT) \
		-predicates ppx_driver \
		-o $@ -linkpkg -linkall \
		-package ppx_tools_versioned \
		$< \
		$(MIGRATE_PARSETREE_MAIN_NATIVE)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj *.annot ppx_sedlex$(EXE) ppx_sedlex.opt$(EXE)
