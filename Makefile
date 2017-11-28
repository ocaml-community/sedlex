# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

VERSION=1.99.5

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

build:
	time -p jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

clean:
	jbuilder clean

doc:
	jbuilder build @doc

test:
	jbuilder build @runtest

all: build doc

.PHONY: build install uninstall clean doc test all package upload

PACKAGE = sedlex-$(VERSION)
DISTRIB = \
  CHANGES LICENSE README.md Makefile \
  examples/jbuild \
  examples/tokenizer.ml \
  src/lib/jbuild \
  src/lib/sedlexing.ml \
  src/lib/sedlexing.mli \
  src/syntax/jbuild \
  src/syntax/sedlex_cset.ml \
  src/syntax/sedlex_cset.mli \
  src/syntax/sedlex.ml \
  src/syntax/sedlex.mli \
  src/syntax/sedlex_ppx.ml \
  src/syntax/unicode63.ml \
  src/syntax/unicode63.mli


package: clean
	rm -rf sedlex.tar.gz
	tar czf sedlex.tar.gz $(DISTRIB)
	rm -Rf $(PACKAGE)
	mkdir $(PACKAGE)
	cd $(PACKAGE) && tar xzf ../sedlex.tar.gz
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	rm -Rf $(PACKAGE) sedlex.tar.gz


TARGET=foo:bar/sedlex_dara
upload: doc
	scp $(PACKAGE).tar.gz README.md CHANGES $(TARGET)/
	rsync -avz _build/default/_doc/  $(TARGET)/libdoc
