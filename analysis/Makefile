SHELL = /bin/bash
MAKEFLAGS += --jobs 4
INCLUDES = -I src -I src/vendor/res_outcome_printer -I src/vendor -I src/vendor/compiler-libs-406

OCAMLOPT = ocamlopt.opt
OCAMLFLAGS = -g -w +26+27+32+33+39 -bin-annot $(INCLUDES)
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
		unix.cmxa str.cmxa $(INCLUDES) $(SOURCE_FILES)
	@echo Done!

build-native: $(OUTPUT) depend

dce: build-native
	../../reanalyze/_build/install/default/bin/reanalyze.exe -dce-cmt src -suppress src/vendor

tests/node_modules/.bin/rescript:
	@cd tests && npm install

test: tests/node_modules/.bin/rescript
	@cd tests && node_modules/.bin/rescript
	./test.sh

clean:
	git clean -dfx .

.DEFAULT_GOAL := build-native

.PHONY: depend clean build-native dce test
