linux: term.ml formula.ml modul.ml prover.ml worker.ml prover_multicore.ml prover_output.ml lexer.mll parser.mly main.ml
	ocamllex lexer.mll       
	ocamlyacc parser.mly 
	ocamlopt -thread -c worker.ml unix.cmxa threads.cmxa
	ocamlopt -thread -o sctl unix.cmxa threads.cmxa term.ml formula.ml modul.ml prover.ml worker.cmx prover_multicore.ml prover_output.ml parser.mli parser.ml lexer.ml main.ml

win: term.ml formula.ml modul.ml prover.ml worker.ml prover_multicore.ml prover_output.ml lexer.mll parser.mly main.ml
	ocamllex lexer.mll       
	ocamlyacc parser.mly 
	ocamlopt -thread -o sctl.exe unix.cmxa threads.cmxa term.ml formula.ml modul.ml prover.ml worker.ml prover_multicore.ml prover_output.ml parser.mli parser.ml lexer.ml main.ml

clean:
	rm -f *.cm[ioax]
	rm -f *.o
	rm -f *.a
	rm -f lexer.ml parser.mli parser.ml
	rm -f *.annot
	rm -f *.bak
	rm -f sctl
	rm -f sctl.exe

