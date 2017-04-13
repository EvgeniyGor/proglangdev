SOURCE=./src/

all: Driver.cmo
	ocamlc -rectypes -o compiler -I `ocamlfind -query GT` GT.cma Expr.cmo StackMachine.cmo X86.cmo Driver.cmo
	rm -Rf *~ *.cm[oi]

Language.cmo: Language.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

Iterpret.cmo: Interpret.ml Language.cmo
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

StackMachine.cmo: StackMachine.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

X86.cmo: X86.ml
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma $<

Driver.cmo: Driver.ml StackMachine.com X86.cmo Interpret.cmo
	ocamlc -rectypes -c -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma Driver.ml


clean:
	rm -Rf *~ *.cm[oi] Driver
