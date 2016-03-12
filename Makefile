SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: setup.ml
	$(SETUP) -clean $(CLEANFLAGS)

distclean: setup.ml
	$(SETUP) -distclean $(DISTCLEANFLAGS)
	rm -f myocamlbuild.ml setup.ml _tags
	rm -f */META */*.mllib */*.mldylib */*.clib

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.ml: _oasis
	oasis setup
	cat _tags.local >> _tags

.PHONY: build doc test all install uninstall reinstall clean distclean configure
