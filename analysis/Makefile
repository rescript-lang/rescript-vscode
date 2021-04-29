SHELL = /bin/bash
MAKEFLAGS += --jobs 4
INCLUDES = -I src -I src/vendor/res_outcome_printer -I src/vendor

OCAMLOPT = ocamlopt.opt
OCAMLFLAGS = -g -w +26+27+32+33+39 -bin-annot -I +compiler-libs $(INCLUDES)
OCAMLDEP = ocamldep.opt

OUTPUT = rescript-editor-analysis.exe

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

$(OUTPUT): $(SOURCE_FILES)
	@echo Linking...
	@$(OCAMLOPT) $(OCAMLFLAGS) -O2 -o $(OUTPUT) \
		-I +compiler-libs unix.cmxa str.cmxa ocamlcommon.cmxa $(INCLUDES) $(SOURCE_FILES)
	@echo Done!

build-native: $(OUTPUT) depend

dce: build-native
	@echo "git diff makefile dce 1"
	git diff tests/src/expected/Auto.res.txt
	../node_modules/.bin/reanalyze -dce-cmt src -suppress src/vendor
	@echo "git diff makefile dce 2"
	git diff tests/src/expected/Auto.res.txt

tests/node_modules/.bin/rescript:
	@echo "git diff makefile rescript 1"
	git diff tests/src/expected/Auto.res.txt
	@cd tests && npm install
	@echo "git diff makefile rescript 2"
	git diff tests/src/expected/Auto.res.txt

test: dce tests/node_modules/.bin/rescript
	@echo "git diff makefile test 1"
	git diff tests/src/expected/Auto.res.txt
	@cd tests && node_modules/.bin/rescript
	@echo "git diff makefile test 2"
	git diff tests/src/expected/Auto.res.txt
	./test.sh

clean:
	git clean -dfx src

foo:
	@echo "git diff makefile foo"
	git diff tests/src/expected/Auto.res.txt

.DEFAULT_GOAL := build-native

.PHONY: depend clean build-native dce test foo
