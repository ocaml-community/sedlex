# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

VERSION=1.99
# Don't forget to change META file as well

.PHONY: all opt clean test install package

all:
	(cd src/lib && make all doc)
	(cd src/syntax && make all)

opt:
	(cd src/lib && make opt)

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
	(cd src/lib && make clean)
	(cd src/syntax && make clean)
	rm -rf libdoc

test: clean all opt
	cd examples && make clean tokenizer.exe && ./tokenizer.exe

install:
	ocamlfind install sedlex META src/syntax/sedlex.exe src/lib/sedlexing.cma src/lib/sedlexing.cmi src/lib/sedlexing.cmx src/lib/sedlexing.cmxa

PACKAGE = sedlex-$(VERSION)
DISTRIB = CHANGES LICENSE META README Makefile
package: clean
	rm -Rf $(PACKAGE)
	mkdir $(PACKAGE)
	cp -R $(DISTRIB) $(PACKAGE)/
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	rm -Rf $(PACKAGE)
