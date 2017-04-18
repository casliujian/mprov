linux: term.ml formula.ml modul.ml prover.ml prover_output.ml lexer.mll parser.mly main.ml
	ocamllex lexer.mll       
	ocamlyacc parser.mly 
	ocamlopt -o sctl term.ml formula.ml modul.ml prover.ml prover_output.ml parser.mli parser.ml lexer.ml main.ml

win: term.ml formula.ml modul.ml prover.ml prover_output.ml lexer.mll parser.mly main.ml
	ocamllex lexer.mll       
	ocamlyacc parser.mly 
	ocamlopt -o sctl.exe term.ml formula.ml modul.ml prover.ml prover_output.ml parser.mli parser.ml lexer.ml main.ml

clean:
	rm -f *.cm[ioax]
	rm -f *.o
	rm -f *.a
	rm -f lexer.ml parser.mli parser.ml
	rm -f *.annot
	rm -f *.bak
	rm -f sctl
	rm -f sctl.exe

