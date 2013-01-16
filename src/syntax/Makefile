SRCS=cset.ml sedlex.mli sedlex.ml sedlex_ppx.ml

all: sedlex.exe

sedlex.exe: $(SRCS)
	ocamlc -annot -o sedlex.exe ocamlcommon.cma -I +compiler-libs $(SRCS)

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
