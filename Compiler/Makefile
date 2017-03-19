all: Driver.cmo
	ocamlc -rectypes -o Driver -I `ocamlfind -query GT` GT.cma Expr.cmo StackMachine.cmo X86.cmo Driver.cmo
	rm -Rf *~ *.cm[oi]

Expr.cmo: Expr.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

StackMachine.cmo: StackMachine.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

X86.cmo: X86.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

Driver.cmo: Expr.cmo StackMachine.cmo X86.cmo Driver.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma Driver.ml


clean:
	rm -Rf *~ *.cm[oi] Driver
