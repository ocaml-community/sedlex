# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

SRCS=cset.mli cset.ml sedlex.mli sedlex.ml sedlex_ppx.ml

all: sedlex.exe sedlex.cma
opt: sedlex.cmxs

sedlex.cma: $(SRCS)
	ocamlc -w +A-4-9 -annot -a -o sedlex.cma -I +compiler-libs $(SRCS)

sedlex.cmxs: $(SRCS)
	ocamlopt -w +A-4-9 -annot -shared -o sedlex.cmxs -I +compiler-libs $(SRCS)

sedlex.exe: sedlex.cma
	ocamlc -w +A-4-9 -annot -o sedlex.exe -linkall -I +compiler-libs ocamlcommon.cma sedlex.cma

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
