all: cset.cmo ulex.cmo ulexing.cmo pa_ulex.cmo

ulexing.cma: utf8.mli utf8.ml ulexing.mli ulexing.ml 
	ocamlc -a -o ulexing.cma utf8.mli utf8.ml ulexing.mli ulexing.ml

pa_ulex.cma: cset.ml ulex.mli ulex.ml pa_ulex.ml
	ocamlc -a -o pa_ulex.cma -pp 'camlp4o pa_extend.cmo q_MLast.cmo' -I +camlp4 cset.ml ulex.mli ulex.ml pa_ulex.ml

clean:
	rm -f *.cm* *~ test *.o

view_test: ulexing.cmo utf8.cmo test.ml
	camlp4o ./pa_ulex.cma pr_o.cmo -sep "\n" test.ml

run_test:
	ocamlc -o test -pp 'camlp4o ./pa_ulex.cma' ulexing.cma test.ml
	./test

doc:
	ocamldoc -html ulexing.mli
