PACKAGES = extlib
FILES = textile.ml

LIBNAME = textile
VERSION :=$(shell head -n 1 VERSION)
DESCRIPTION = textile markup language support

CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt -g $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep

DUCEFIND = ocamlducefind
DUCEC   = $(DUCEFIND) ocamlc -g -thread $(LIB)
DUCEOPT = $(DUCEFIND) ocamlopt -g -thread $(LIB)
DUCEDEP = $(DUCEFIND) ocamldep
DUCEDOC = $(DUCEFIND) ocamldoc $(LIB)

LIB = -package $(PACKAGES)
PP =

OBJS = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

CMA = textile.cma
CMXA = textile.cmxa

all: byte native duce

ifndef DUCE
  DUCE = yes
endif

ifeq "$(DUCE)" "yes"
  MAKEDOCS = $(CAMLDOC) -d doc -html *.mli
  MAKEDUCE = make -C duce; cp duce/{*.cmi,*.cma,*.cmxa,*.a} ./
  CLEANDUCE = make -C duce clean
else
  MAKEDOCS = $(DUCEDOC) -d doc -html *.mli duce/*.mli
  MAKEDUCe =
  CLEANDUCE =
endif

META: META.in VERSION duce/META.in
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

duce: all
	$(MAKEDUCE)

install:
	ocamlfind install $(LIBNAME) META *.cmi *.cma *.cmxa *.a

uninstall:
	ocamlfind remove $(LIBNAME)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc duce

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	-mkdir -p doc
	$(MAKEDOCS)

clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmxa *~ $(NAME)
	-rm -f .depend
	-rm -rf doc
	-rm -f META
	-rm -f itextile
	$(CLEANDUCE)

depend: .depend

.depend: $(FILES)
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
