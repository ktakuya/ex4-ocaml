.PHONY: clean

OBJS=mySet.cmo syntax.cmo parser.cmo lexer.cmo environment.cmo typing.cmo eval.cmo

test-type-inference: typeinference.o
	./typeinference.o

typeinference.o: typeinference.ml
	cd ../type-inference && make depend && make
	ocamlfind ocamlc -o typeinference.o -package oUnit -linkpkg -I ../type-inference $(OBJS) -g typeinference.ml 

clean:
	rm *.cm[io] *.o *.cache *.log
