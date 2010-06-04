ifndef DUCE
  DUCE := $(shell if [ `which ocamlduce` ]; then echo yes; else echo no; fi)
endif

LIBNAME = textile
VERSION :=$(shell head -n 1 VERSION)

PACKAGES = extlib
FILES = textile.ml
DUCEPACKAGES = ocamlduce,ocsigen
DUCEFILES = textile_duce.ml

ifeq "$(DUCE)" "yes"
  CAMLC   = ocamlducefind ocamlc -g -thread $(LIB)
  CAMLOPT = ocamlducefind ocamlopt -g -thread $(LIB)
  CAMLDOC = ocamlducefind ocamldoc $(LIB)
  CAMLDEP = ocamlducefind ocamldep
  LIB = -package $(PACKAGES),$(DUCEPACKAGES)
  OTHERFILES = itextile.ml xhtmlpretty_duce.ml itexilte_duce.ml
else
  CAMLC   = ocamlfind ocamlc -g $(LIB)
  CAMLOPT = ocamlfind ocamlopt -g $(LIB)
  CAMLDOC = ocamlfind ocamldoc $(LIB)
  CAMLDEP = ocamlfind ocamldep
  LIB = -package $(PACKAGES)
  OTHERFILES = itextile.ml
endif

OBJS = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)
DUCEOBJS = $(DUCEFILES:.ml=.cmo)
DUCEOPTOBJS = $(DUCEFILES:.ml=.cmx)

CMA  = textile.cma
CMXA = textile.cmxa
CMXS = textile.cmxs
DUCECMA  = textile_duce.cma
DUCECMXA = textile_duce.cmxa
DUCECMXS = textile_duce.cmxs

all: byte native shared

ifeq "$(DUCE)" "yes"
byte:   depend $(CMA)  $(DUCECMA)  META
native: depend $(CMXA) $(DUCECMXA) META
shared: depend $(CMXS) $(DUCECMXS) META
else
byte:   depend $(CMA)  META
native: depend $(CMXA) META
shared: depend $(CMXS) META
endif

META: META.in META.duce.in VERSION
	cp META.in META
	sed "s/_NAME_/$(LIBNAME)/" -i META
	sed "s/_VERSION_/$(VERSION)/" -i META
	sed "s/_REQUIRES_/$(PACKAGES)/" -i META
	sed "s/_BYTE_/$(CMA)/" -i META
	sed "s/_NATIVE_/$(CMXA)/" -i META
	sed "s/_SHARED_/$(CMXS)/" -i META
ifeq "$(DUCE)" "yes"
	cp META.duce.in META.duce
	sed "s/_NAME_/duce/" -i META.duce
	sed "s/_REQUIRES_/$(DUCEPACKAGES)/" -i META.duce
	sed "s/_BYTE_/$(DUCECMA)/" -i META.duce
	sed "s/_NATIVE_/$(DUCECMXA)/" -i META.duce
	sed "s/_SHARED_/$(DUCECMXS)/" -i META.duce
	cat META.duce >> META
endif

$(CMA): $(OBJS)
	$(CAMLC) -a -o $@ $(OBJS)
$(DUCECMA): $(DUCEOBJS)
	$(CAMLC) -a -o $@ $(DUCEOBJS)

$(CMXA): $(OPTOBJS)
	$(CAMLOPT) -a -o $@ $(OPTOBJS)
$(DUCECMXA): $(DUCEOPTOBJS)
	$(CAMLOPT) -a -o $@ $(DUCEOPTOBJS)

$(CMXS): $(OPTOBJS)
	$(CAMLOPT) -shared -o $(CMXS) $(OPTOBJS)
$(DUCECMXS): $(DUCEOPTOBJS)
	$(CAMLOPT) -shared -o $(CMXS) $(DUCEOPTOBJS)

itextile.cmx: $(CMXA) itextile.ml
	$(CAMLOPT) $(CMXA) -c itextile.ml
itextile: $(CMXA) itextile.cmx
	$(CAMLOPT) -linkpkg -o $@ $^

itextile_duce.cmx: $(DUCECMXA) itextile_duce.ml xhtmlpretty_duce.cmx
	$(CAMLOPT) -c itextile_duce.ml
itextile_duce: $(CMXA) $(DUCECMXA) xhtmlpretty_duce.cmx itextile_duce.cmx
	$(CAMLOPT) -linkpkg -o $@ $^

tests: itextile itextile_duce
	make -C tests/

install:
	ocamlfind install $(LIBNAME) META *.cmi *.cma *.cmxa *.cmxs *.a

uninstall:
	ocamlfind remove $(LIBNAME)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) -c $<

doc:
	-mkdir -p doc
ifeq "$(DUCE)" "yes"
	$(CAMLDOC) -d doc -html $(FILES:.ml=.mli) $(DUCEFILES:.ml=.mli)
else
	$(CAMLDOC) -d doc -html $(FILES:.ml=.mli)
endif


clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmx[as] *~
	-rm -f .depend
	-rm -rf doc
	-rm -f META META.duce
	-rm -f itextile itextile_duce
	make -C tests/ clean

depend: .depend

ifeq "$(DUCE)" "yes"
.depend: $(FILES) $(DUCEFILES)
	$(CAMLDEP) $(LIB) $(FILES:.ml=.mli) $(FILES) $(OTHERFILES:.ml=.mli) $(OTHERFILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend
else
.depend: $(FILES)
	$(CAMLDEP) $(LIB) $(OTHERFILES:.ml=.mli) $(OTHERFILES) $(FILES:.ml=.mli) $(FILES) > .depend
endif

FORCE:

-include .depend
