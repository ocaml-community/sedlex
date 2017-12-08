# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: build install uninstall clean doc test all

build:
	jbuilder build @install

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
