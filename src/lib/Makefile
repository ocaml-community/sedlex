MODS=utf8.cmo sedlexing.cmo utf16.cmo

all: sedlexing.cma

sedlexing.cma: $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlc -c $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlc -a -o sedlexing.cma $(MODS)

sedlexing.cmxa: $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlopt -c $(MODS:.cmo=.mli) $(MODS:.cmo=.ml)
	ocamlopt -a -o sedlexing.cmxa $(MODS:.cmo=.cmx)


clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
