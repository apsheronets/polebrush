ifndef DUCE
  DUCE := $(shell if [ `which ocamlduce` ]; then echo yes; else echo no; fi)
endif

LIBNAME = polebrush
VERSION := $(shell head -n 1 VERSION)

POLEBRUSH = polebrush_parsercomb.ml polebrush.ml polebrush_parser.ml
HTML      = polebrush_html.ml
DUCEF     = polebrush_duce.ml

CAMLSRC = polebrush_parsercomb.ml polebrush.ml polebrush_parser.ml polebrush_html.ml
DUCESRC = xhtmlpretty_duce.ml polebrush_duce.ml polebrush_duce_cmd.ml

DOCSRC = polebrush.ml polebrush_parser.ml polebrush_html.ml polebrush.mli polebrush_parser.mli polebrush_html.mli
DUCEDOCSRC = $(DOCSRC) polebrush_duce.ml polebrush_duce.mli

PACKAGES = extlib
DUCEPACKAGES = ocamlduce,ocsigen

DUCELIB = -package $(PACKAGES),$(DUCEPACKAGES)
DUCEC   = ocamlducefind ocamlc   -thread $(DUCELIB)
DUCEOPT = ocamlducefind ocamlopt -thread $(DUCELIB)
DUCEDOC = ocamlducefind ocamldoc -keep-code -colorize-code $(DUCELIB)
DUCEDEP = ocamlducefind ocamldep

LIB = -package $(PACKAGES)
CAMLC   = ocamlfind ocamlc   $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc -keep-code -colorize-code $(LIB)
CAMLDEP = ocamlfind ocamldep

CMA  = polebrush.cma
CMXA = polebrush.cmxa
CMXS = polebrush.cmxs
DUCECMA  = polebrush_duce.cma
DUCECMXA = polebrush_duce.cmxa
DUCECMXS = polebrush_duce.cmxs

all: libs tools
tools: polebrush polebrush.byte
libs: bytelib nativelib sharedlib

ifeq "$(DUCE)" "yes"
bytelib:             polebrush.cma  polebrush_html.cma  polebrush_duce.cma  META
nativelib:           polebrush.cmxa polebrush_html.cmxa polebrush_duce.cmxa META
sharedlib: nativelib polebrush.cmxs polebrush_html.cmxs polebrush_duce.cmxs META
else
bytelib:             polebrush.cma  polebrush_html.cma  META
nativelib:           polebrush.cmxa polebrush_html.cmxa META
sharedlib: nativelib polebrush.cmxs polebrush_html.cmxs META
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

polebrush.cma:  $(POLEBRUSH:.ml=.cmo)
	$(CAMLC) -a -o $@ $^
polebrush.cmxa: $(POLEBRUSH:.ml=.cmx)
	$(CAMLOPT) -a -o $@ $^
polebrush.cmxs: $(POLEBRUSH:.ml=.cmx) polebrush.cmxa
	$(CAMLOPT) -a -o $@ $(POLEBRUSH:.ml=.cmx)

polebrush_html.cma:  $(HTML:.ml=.cmo)
	$(CAMLC) -a -o $@ $^
polebrush_html.cmxa: $(HTML:.ml=.cmx)
	$(CAMLOPT) -a -o $@ $^
polebrush_html.cmxs: $(HTML:.ml=.cmx) polebrush_html.cmxa
	$(CAMLOPT) -a -o $@ polebrush_html.cmx

polebrush_duce.cmo: polebrush_duce.ml
	$(DUCEC) -c $<
polebrush_duce.cmi: polebrush_duce.mli
	$(DUCEC) -c $<
polebrush_duce.cmx: polebrush_duce.ml
	$(DUCEOPT) -c $<

polebrush_duce.cma:  $(DUCEF:.ml=.cmo)
	$(DUCEC) -a -o $@ $^
polebrush_duce.cmxa: $(DUCEF:.ml=.cmx)
	$(DUCEOPT) -a -o $@ $^
polebrush_duce.cmxs: $(DUCEF:.ml=.cmx) polebrush_duce.cmxa
	$(DUCEOPT) -a -o $@ $(DUCEF:.ml=.cmx)

polebrush_cmd.cmx: polebrush.cmxa polebrush_html.cmxa polebrush_cmd.ml
	$(CAMLOPT) -c $^
polebrush: polebrush.cmxa polebrush_html.cmxa polebrush_cmd.cmx
	$(CAMLOPT) -linkpkg -o $@ $^
polebrush_cmd.cmo: polebrush.cma polebrush_html.cma polebrush_cmd.ml
	$(CAMLC) -c $^
polebrush.byte: polebrush.cma polebrush_html.cma polebrush_cmd.cmo
	$(CAMLC) -linkpkg -o $@ $^

xhtmlpretty_duce.cmi: xhtmlpretty_duce.mli
	$(DUCEC) -c $<
xhtmlpretty_duce.cmx: xhtmlpretty_duce.ml
	$(DUCEOPT) -c $<
polebrush_duce_cmd.cmx: polebrush_duce_cmd.ml
	$(DUCEOPT) -c $<
polebrush_duce: xhtmlpretty_duce.cmx polebrush.cmxa polebrush_duce.cmxa polebrush_duce_cmd.cmx
	$(DUCEOPT) -linkpkg -o $@ $^

tests: polebrush_cmd polebrush_duce_cmd

install-lib:
	ocamlfind install $(LIBNAME) META *.cmi *.cma *.cmxa *.cmxs *.a

uninstall-lib:
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
	$(DUCEDOC) -d doc -html $(DUCEDOCSRC)
else
	$(CAMLDOC) -d doc -html $(DOCSRC)
endif


clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmx[as] *~
	-rm -f .depend
	-rm -rf doc
	-rm -f META META.duce
	-rm -f polebrush polebrush.byte polebrush_duce

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
