PACKAGES = extlib
FILES = textile.ml

CAMLC = ocamlfind ocamlc -thread -g $(LIB)
CAMLOPT = ocamlfind ocamlopt -thread -g $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package $(PACKAGES)
#PP = -pp "camlp4o"
PP =

OBJS = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

CMA = textile.cma
CMXS = textile.cmxs

all: byte native

byte: depend $(CMA) install

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)

native: depend $(CMXS) installopt

$(CMXS): $(OPTOBJS)
	$(CAMLOPT) -shared -o $(CMXS) $(OPTOBJS)

install:
#	chmod a+r $(CMA)

installopt:
#	chmod a+r $(CMXS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	-mkdir -p doc
	$(CAMLDOC) -d doc -html *.mli

clean:
	-rm -f *.cm[ioxa] *.o $(CMXS) *~ $(NAME)
	-rm -rf doc

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
