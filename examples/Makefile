tokenizer.exe: tokenizer.ml
	ocamlc -ppx "../src/syntax/sedlex.exe" -I ../src/lib -o tokenizer.exe sedlexing.cma tokenizer.ml

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
