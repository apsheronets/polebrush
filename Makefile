PACKAGES = extlib
FILES = textile.ml

LIBNAME = textile
VERSION :=$(shell head -n 1 VERSION)
DESCRIPTION = textile markup language support

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

META: META.in VERSION
	cp $< $@
	sed "s/_NAME_/$(LIBNAME)/" -i $@
	sed "s/_VERSION_/$(VERSION)/" -i $@
	sed "s/_DESCRIPTION_/$(DESCRIPTION)/" -i $@
	sed "s/_REQUIRES_/$(PACKAGES)/" -i $@
	sed "s/_BYTE_/$(CMA)/" -i $@
	sed "s/_NATIVE_/$(CMXA)/" -i $@

byte: depend $(CMA) META

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)

native: depend $(CMXA) META

$(CMXA): $(OPTOBJS)
	$(CAMLOPT) -a -o $(CMXA) $(OPTOBJS)

itextile.cmx: $(CMXA) itextile.ml
	$(CAMLOPT) $(CMXA) -c itextile.ml

itextile: $(CMXA) itextile.cmx
	$(CAMLOPT) -linkpkg -o $@ $^

install:
	ocamlfind install $(LIBNAME) META *.cmi *.cma $(MLI) $(wildcard *.cmxa) $(wildcard *.a)

uninstall:
	ocamlfind remove $(LIBNAME)

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
	-rm -f *.cm[ioxa] *.o *.a $(CMXA) *~ $(NAME)
	-rm -f .depend
	-rm -rf doc
	-rm -f META
	-rm -f itextile

depend: .depend

.depend: $(FILES)
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
