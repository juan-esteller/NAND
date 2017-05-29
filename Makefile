MAIN= exec

OBJS = nand.cmo NANDlexer.cmo NANDparser.cmo eval.cmo
%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


$(MAIN): clean $(OBJS)
	ocamlc -g -o $(MAIN) $(OBJS)

NANDlexer.ml : NANDlexer.mll
	ocamllex -q $<

NANDlexer.cmo : NANDparser.cmi NANDlexer.ml
	ocamlc -g -c NANDlexer.ml

NANDparser.ml : NANDparser.mly
	ocamlyacc -q $<

NANDparser.mli : NANDparser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi NANDlexer.ml NANDparser.ml NANDparser.mli $(MAIN)
