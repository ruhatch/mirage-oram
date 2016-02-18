# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

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

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

report_dir:
	mkdir report_dir

report: report_dir
	find tests -name 'bisect*' | xargs rm -f
	make test
	bisect-report tests/bisect*.out -I _build -html report_dir

clean_report:
	rm -rf report_dir
	find tests -name 'bisect*' | xargs rm -f

clean_tests:
	rm -rf _tests
	rm -rf tests/_tests

clean_all: clean clean_report clean_tests

mirage: build reinstall
	(cd oram-client; make clean; make build; ./mir-oram-client)
