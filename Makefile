ifndef DUCE
  DUCE := $(shell if [ `which ocamlduce` ]; then echo yes; else echo no; fi)
endif

LIBNAME = textile
VERSION := $(shell head -n 1 VERSION)

TEXTILE = parsercomb.ml textile.ml
HTML    = textile_html.ml
DUCEF   = textile_duce.ml

CAMLSRC = parsercomb.ml textile.ml textile_parser.ml textile_html.ml
DUCESRC = xhtmlpretty_duce.ml textile_duce.ml

DOCSRC = textile.mli textile_parser.mli textile_html.mli
DUCEDOCSRC = $(DOCSRC) textile_duce.mli

PACKAGES = extlib
DUCEPACKAGES = ocamlduce,ocsigen

DUCELIB = -package $(PACKAGES),$(DUCEPACKAGES)
DUCEC   = ocamlducefind ocamlc   -thread $(DUCELIB)
DUCEOPT = ocamlducefind ocamlopt -thread $(DUCELIB)
DUCEDOC = ocamlducefind ocamldoc $(DUCELIB)
DUCEDEP = ocamlducefind ocamldep

LIB = -package $(PACKAGES)
CAMLC   = ocamlfind ocamlc $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
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
byte:          textile.cma  textile_html.cma  textile_duce.cma  META
native:        textile.cmxa textile_html.cmxa textile_duce.cmxa META
shared: native textile.cmxs textile_html.cmxs textile_duce.cmxs META
else
byte:          textile.cma  textile_html.cma  META
native:        textile.cmxa textile_html.cmxa META
shared: native textile.cmxs textile_html.cmxs META
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

textile.cma:  parsercomb.cmo textile.cmo textile_parser.cmo
	$(CAMLC) -a -o $@ $^
textile.cmxa: parsercomb.cmx textile.cmx textile_parser.cmx
	$(CAMLOPT) -a -o $@ $^
textile.cmxs: parsercomb.cmx textile.cmx textile_parser.cmx
	$(CAMLOPT) -a -o $@ $^

textile_html.cma:  textile_html.cmo
	$(CAMLC) -a -o $@ $^
textile_html.cmxa: textile_html.cmx
	$(CAMLOPT) -a -o $@ $^
textile_html.cmxs: textile_html.cmx
	$(CAMLOPT) -a -o $@ $^

textile_duce.cmo: textile_duce.ml
	$(DUCEC) -c $<
textile_duce.cmi: textile_duce.mli
	$(DUCEC) -c $<
textile_duce.cmx: textile_duce.ml
	$(DUCEOPT) -c $<

textile_duce.cma:  textile_duce.cmo
	$(DUCEC) -a -o $@ $^
textile_duce.cmxa: textile_duce.cmx
	$(DUCEOPT) -a -o $@ $^
textile_duce.cmxs: textile_duce.cmx
	$(DUCEOPT) -a -o $@ $^

itextile.cmx: textile.cmxa textile_html.cmxa itextile.ml
	$(CAMLOPT) -c $^
itextile: textile.cmxa textile_html.cmxa itextile.cmx
	$(CAMLOPT) -linkpkg -o $@ $^
itextile.cmo: textile.cma textile_html.cma itextile.ml
	$(CAMLC) -c $^
itextile.byte: textile.cma textile_html.cma itextile.cmo
	$(CAMLC) -linkpkg -o $@ $^

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
	$(DUCEDOC) -d doc -html $(DUCEDOCSRC)
else
	$(CAMLDOC) -d doc -html $(DOCSRC)
endif


clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmx[as] *~
	-rm -f .depend
	-rm -rf doc
	-rm -f META META.duce
	-rm -f itextile itextile.byte itextile_duce
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
