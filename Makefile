VERSION=1.99
# Don't forget to change META file as well

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
	(cd src/lib && make clean)
	(cd src/syntax && make clean)

all:
	(cd src/lib && make all)
	(cd src/syntax && make all)

test: clean all
	cd examples && make clean tokenizer.exe && ./tokenizer.exe

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
