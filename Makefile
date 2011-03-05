ifndef DUCE
  DUCE := $(shell if [ `which ocamlduce` ]; then echo yes; else echo no; fi)
endif

LIBNAME = textile
VERSION := $(shell head -n 1 VERSION)

TEXTILE = textile
HTML    = textile_html
DUCEF   = textile_duce

CAMLSRC=$(TEXTILE).ml $(HTML).ml
DUCESRC=xhtmlpretty_duce.ml $(DUCEF).ml

PACKAGES = extlib
DUCEPACKAGES = ocamlduce,ocsigen

DUCELIB = -package $(PACKAGES),$(DUCEPACKAGES)
DUCEC   = ocamlducefind ocamlc   -g -thread $(DUCELIB)
DUCEOPT = ocamlducefind ocamlopt -g -thread $(DUCELIB)
DUCEDOC = ocamlducefind ocamldoc $(DUCELIB)
DUCEDEP = ocamlducefind ocamldep

LIB = -package $(PACKAGES)
CAMLC   = ocamlfind ocamlc   -g $(LIB)
CAMLOPT = ocamlfind ocamlopt -g $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep

CMA  = textile.cma
CMXA = textile.cmxa
CMXS = textile.cmxs
DUCECMA  = textile_duce.cma
DUCECMXA = textile_duce.cmxa
DUCECMXS = textile_duce.cmxs

all: byte native shared

ifeq "$(DUCE)" "yes"
byte:   $(TEXTILE).cma  $(HTML).cma  $(DUCEF).cma  META
native: $(TEXTILE).cmxa $(HTML).cmxa $(DUCEF).cmxa META
shared: $(TEXTILE).cmxs $(HTML).cmxs $(DUCEF).cmxs META
else
byte:   $(TEXTILE).cma  $(HTML).cma  META
native: $(TEXTILE).cmxa $(HTML).cmxa META
shared: $(TEXTILE).cmxs $(HTML).cmxs META
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

$(TEXTILE).cma:  $(TEXTILE).cmo
	$(CAMLC) -a -o $@ $^
$(TEXTILE).cmxa: $(TEXTILE).cmx
	$(CAMLOPT) -a -o $@ $^
$(TEXTILE).cmxs: $(TEXTILE).cmx
	$(CAMLOPT) -a -o $@ $^

$(HTML).cma:  $(HTML).cmo
	$(CAMLC) -a -o $@ $^
$(HTML).cmxa: $(HTML).cmx
	$(CAMLOPT) -a -o $@ $^
$(HTML).cmxs: $(HTML).cmx
	$(CAMLOPT) -a -o $@ $^

$(DUCEF).cmo: $(DUCEF).ml
	$(DUCEC) -c $<
$(DUCEF).cmi: $(DUCEF).mli
	$(DUCEC) -c $<
$(DUCEF).cmx: $(DUCEF).ml
	$(DUCEOPT) -c $<

$(DUCEF).cma:  $(DUCEF).cmo
	$(DUCEC) -a -o $@ $^
$(DUCEF).cmxa: $(DUCEF).cmx
	$(DUCEOPT) -a -o $@ $^
$(DUCEF).cmxs: $(DUCEF).cmx
	$(DUCEOPT) -a -o $@ $^

itextile.cmx: native itextile.ml
	$(CAMLOPT) -I ./ -package textile,textile.html -c itextile.ml
itextile: native itextile.cmx
	$(CAMLOPT) -I ./ -package textile,textile.html -linkpkg -o $@ itextile.cmx

xhtmlpretty_duce.cmi: xhtmlpretty_duce.mli
	$(DUCEC) -c $<
xhtmlpretty_duce.cmx: xhtmlpretty_duce.ml
	$(DUCEOPT) -c $<
itextile_duce.cmx: native itextile_duce.ml xhtmlpretty_duce.cmx
	$(DUCEOPT) -I ./ -package textile.duce -c itextile_duce.ml
itextile_duce: xhtmlpretty_duce.cmx itextile_duce.cmx
	$(DUCEOPT) -I ./ -package textile.duce -linkpkg -o $@ $^

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

doc: byte
	-mkdir -p doc
ifeq "$(DUCE)" "yes"
	$(DUCEDOC) -d doc -html $(CAMLSRC:.ml=.mli) $(DUCEF).mli
else
	$(CAMLDOC) -d doc -html $(CAMLSRC:.ml=.mli)
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
.depend: $(CAMLSRC) $(DUCESRC)
	$(DUCEDEP) $(LIB) $(CAMLSRC:.ml=.mli) $(CAMLSRC) $(DUCESRC:.ml=.mli) $(DUCESRC) > .depend
else
.depend: $(CAMLSRC)
	$(CAMLDEP) $(LIB) $(CAMLSRC:.ml=.mli) $(CAMLSRC) > .depend
endif

FORCE:

-include .depend
