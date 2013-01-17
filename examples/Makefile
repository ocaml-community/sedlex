# The package sedlex is released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2005, 2013 by Alain Frisch and LexiFi.

tokenizer.exe: tokenizer.ml
	ocamlc -ppx "../src/syntax/sedlex.exe" -I ../src/lib -o tokenizer.exe sedlexing.cma tokenizer.ml

clean:
	rm -f *~ *.cm* *.a *.lib *.exe *.o *.obj
