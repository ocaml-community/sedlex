# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

VERSION=1.99.1
# Don't forget to change META file as well

.PHONY: all opt clean test install package

all:
	(cd src/lib && make all doc)
	(cd src/syntax && make all)

opt:
	(cd src/syntax && make opt)
	(cd src/lib && make opt)

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj
	(cd src/lib && make clean)
	(cd src/syntax && make clean)
	(cd examples && make clean)
	rm -rf libdoc

test: clean all opt
	cd examples && make test

INSTALL=META src/syntax/sedlex.cma src/syntax/ppx_sedlex$(EXE) src/lib/sedlexing.cma src/lib/sedlexing.cmi

INSTALL_OPT=src/syntax/sedlex.cmxs src/syntax/sedlex$(EXT_LIB) src/syntax/sedlex.cmxa src/syntax/ppx_sedlex.opt$(EXE) src/lib/sedlexing.cmx src/lib/sedlexing$(EXT_LIB) src/lib/sedlexing.cmxa

install:
	ocamlfind install sedlex $(INSTALL) $(INSTALL_OPT)

install_byteonly:
	ocamlfind install sedlex $(INSTALL)

uninstall:
	ocamlfind remove sedlex

PACKAGE = sedlex-$(VERSION)
DISTRIB = \
  CHANGES LICENSE META README Makefile \
  examples/Makefile \
  examples/tokenizer.ml \
  src/lib/Makefile \
  src/lib/sedlexing.ml \
  src/lib/sedlexing.mli \
  src/syntax/Makefile \
  src/syntax/cset.ml \
  src/syntax/cset.mli \
  src/syntax/sedlex.ml \
  src/syntax/sedlex.mli \
  src/syntax/sedlex_ppx.ml \


package: clean
	rm -rf sedlex.tar.gz
	tar czf sedlex.tar.gz $(DISTRIB)
	rm -Rf $(PACKAGE)
	mkdir $(PACKAGE)
	cd $(PACKAGE) && tar xzf ../sedlex.tar.gz
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	rm -Rf $(PACKAGE) sedlex.tar.gz


TARGET=foo:bar/sedlex_dara
upload:
	scp $(PACKAGE).tar.gz README CHANGES $(TARGET)/
	rsync -avz libdoc $(TARGET)
