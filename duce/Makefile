PACKAGES = textile,ocamlduce,ocsigen
FILES = textile_duce.ml

LIBNAME = textile_duce
VERSION :=$(shell head -n 1 ../VERSION)
DESCRIPTION = addition for package textile: ocamlduce XHTML support for ocsigen

DUCEFIND = ocamlducefind
DUCEC   = $(DUCEFIND) ocamlc -g -thread $(LIB)
DUCEOPT = $(DUCEFIND) ocamlopt -g -thread $(LIB)
DUCEDEP = $(DUCEFIND) ocamldep
DUCEDOC = $(DUCEFIND) ocamldoc $(LIB)

LIB = -I ../ -package $(PACKAGES)

OBJS = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

CMA = $(LIBNAME).cma
CMXA = $(LIBNAME).cmxa

all: byte native

META: META.in ../VERSION
	cp $< $@
	sed "s/_NAME_/$(LIBNAME)/" -i $@
	sed "s/_VERSION_/$(VERSION)/" -i $@
	sed "s/_DESCRIPTION_/$(DESCRIPTION)/" -i $@
	sed "s/_REQUIRES_/$(PACKAGES)/" -i $@
	sed "s/_BYTE_/$(CMA)/" -i $@
	sed "s/_NATIVE_/$(CMXA)/" -i $@

byte: depend $(CMA) META

$(CMA): $(OBJS)
	$(DUCEC) -a -o $(CMA) $(OBJS)

native: depend $(CMXA) META

$(CMXA): $(OPTOBJS)
	$(DUCEOPT) -a -o $(CMXA) $(OPTOBJS)

itextile_duce.cmx: $(CMXA) itextile_duce.ml xhtmlpretty_duce.cmx
	$(DUCEOPT) $(CMXA) -c itextile_duce.ml

itextile_duce: $(CMXA) xhtmlpretty_duce.cmx itextile_duce.cmx
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
	-rm -f *.cm[ioxa] *.a *.o $(CMA) $(CMXA) *~ .depend META
	-rm -rf doc
	-rm -f itextile_duce

depend: .depend

.depend: $(FILES)
	$(DUCEDEP) $(LIB) *.mli *.ml > .depend

FORCE:

-include .depend


