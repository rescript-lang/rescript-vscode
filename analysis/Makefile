SHELL = /bin/bash
MAKEFLAGS += --jobs 4
INCLUDES = -I src -I src/vendor/odoc_parser -I src/vendor/omd -I src/vendor/res_outcome_printer -I src/vendor

OCAMLOPT = ocamlopt.opt
OCAMLFLAGS = -g -w +26+27+32+33+39 -bin-annot -I +compiler-libs $(INCLUDES)
OCAMLDEP = ocamldep.opt

%.cmi : %.mli
	@echo Building $@
	@$(OCAMLOPT) $(OCAMLFLAGS) -c $<
%.cmx : %.ml
	@echo Building $@
	@$(OCAMLOPT) $(OCAMLFLAGS) -c $<

include .depend
depend:
	@$(OCAMLDEP) -native $(INCLUDES) `find src -name "*.ml" -o -name "*.mli"` > .depend

SOURCE_FILES = $(shell $(OCAMLDEP) -sort `find src -name "*.ml"` | sed -E "s/\.ml/.cmx/g")

lib/rescript-editor-support.exe: $(SOURCE_FILES)
	@echo Linking...
	@$(OCAMLOPT) $(OCAMLFLAGS) -O2 -o ./lib/rescript-editor-support.exe \
		-I +compiler-libs unix.cmxa str.cmxa ocamlcommon.cmxa $(INCLUDES) $(SOURCE_FILES)
	@echo Done!

build-native: lib/rescript-editor-support.exe depend

dce: build-native
	node_modules/.bin/reanalyze -dce-cmt src -suppress src/vendor

tests/node_modules/.bin/rescript:
	@cd tests && npm install

tests/lib/.compiler.log: tests/node_modules/.bin/rescript
	@cd tests && node_modules/.bin/rescript build -with-deps

test: dce tests/lib/.compiler.log
	./test.sh

clean:
	git clean -dfx src

.DEFAULT_GOAL := build-native

.PHONY: depend clean build-native dce test
