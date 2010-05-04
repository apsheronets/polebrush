PACKAGES = unix
FILES = *.ml

CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt -g $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep

LIB = -package $(PACKAGES)
PP =

OBJS = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

CMA = textile.cma
CMXA = textile.cmxa

all: byte native

byte: depend compare

native: depend compare.opt

compare: compare.cmo
	$(CAMLC) -linkpkg -o $@ $^

compare.opt: compare.cmx
	$(CAMLOPT) -linkpkg -o $@ $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmxa *~ $(NAME)
	-rm -f .depend
	-rm -f compare compare.opt
	$(CLEANDUCE)

depend: .depend

.depend: $(FILES)
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
