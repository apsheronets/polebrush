PACKAGES = textile,ocamlduce
FILES = xhtmltypes_duce.ml xhtmlpretty_duce.ml textile_duce.ml

LIBNAME = textile_duce

DUCEFIND = ocamlducefind
DUCEC   = $(DUCEFIND) ocamlc -g $(LIB)
DUCEOPT = $(DUCEFIND) ocamlopt -g $(LIB)
DUCEDEP = $(DUCEFIND) ocamldep
DUCEDOC = $(DUCEFIND) ocamldoc $(LIB)

OCAMLFIND = ocamlfind
CAMLC   = $(OCAMLFIND) ocamlc -g $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt -g $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)

LIB = -I ../ -package $(PACKAGES)

OBJS = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

CMA = $(LIBNAME).cma
CMXA = $(LIBNAME).cmxa

all: byte native

byte: depend $(CMA)

$(CMA): $(OBJS)
	$(DUCEC) -a -o $(CMA) $(OBJS)

native: depend $(CMXA)

$(CMXA): $(OPTOBJS)
	$(DUCEOPT) -a -o $(CMXA) $(OPTOBJS)

itextile_duce.cmx: $(CMXA) itextile_duce.ml
	$(DUCEOPT) $(CMXA) -c itextile_duce.ml

itextile_duce: $(CMXA) itextile_duce.cmx
	$(DUCEOPT) -linkpkg -o $@ $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: doc

.ml.cmo:
	$(DUCEC) -c $<

.mli.cmi:
	$(DUCEC) -c $<
.ml.cmx:
	$(DUCEOPT) -c $<

clean:
	-rm -f *.cm[ioxa] *.a *.o $(CMA) $(CMXA) *~ .depend
	-rm itextile_duce

depend: .depend

.depend: $(FILES)
	$(DUCEDEP) $(LIB) *.mli *.ml > .depend

FORCE:

-include .depend


