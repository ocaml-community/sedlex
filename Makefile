all: cset.cmo ulex.cmo ulexing.cmo pa_ulex.cmo

cset.cmo: cset.ml
	ocamlc -c cset.ml

ulex.cmi: ulex.mli
	ocamlc -c ulex.mli

ulex.cmo: ulex.cmi ulex.ml
	ocamlc -c ulex.ml


ulexing.cmi: ulexing.mli
	ocamlc -c ulexing.mli

ulexing.cmo: ulexing.cmi ulexing.ml
	ocamlc -c ulexing.ml

pa_ulex.cmo: pa_ulex.ml
	ocamlc -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo' -I +camlp4 pa_ulex.ml

clean:
	rm -f *.cm* *~

test: ulexing.cmo test.ml
	camlp4o ./cset.cmo ./ulex.cmo ./pa_ulex.cmo pr_o.cmo -sep "\n" test.ml
	ocamlc -o test -pp 'camlp4o ./cset.cmo ./ulex.cmo ./pa_ulex.cmo' ulexing.cmo test.ml

doc:
	ocamldoc -html ulexing.mli
