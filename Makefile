VERSION=1.99
# Don't forget to change META file as well

MODS=utf8.cmo ulexing.cmo utf16.cmo

all: ulexing.cma

ulexing.cma:
	ocamlc -c $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlc -a -o ulexing.cma $(MODS)

ulexing.cmxa:
	ocamlopt -c $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlopt -a -o ulexing.cmxa $(MODS:.cmo=.cmx)


clean:
	rm -f *~ *.cm* *.a *.lib *.exe
doc:
	ocamldoc -html ulexing.mli

PACKAGE = sedlex-$(VERSION)
DISTRIB = CHANGES LICENSE META README Makefile _tags *.ml *.mli
.PHONY: package
package: clean
	rm -Rf $(PACKAGE)
	mkdir $(PACKAGE)
	cp -R $(DISTRIB) $(PACKAGE)/
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	rm -Rf $(PACKAGE)
