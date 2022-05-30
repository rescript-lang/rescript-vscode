SHELL = /bin/bash

build-native:
	rm -f rescript-editor-analysis.exe
	dune build
	cp _build/install/default/bin/rescript-editor-analysis rescript-editor-analysis.exe

dce: build-native
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

tests/node_modules/.bin/rescript:
	@cd tests && npm install

test-analysis-binary: build-native tests/node_modules/.bin/rescript
	@cd tests && node_modules/.bin/rescript
	./test.sh

test-reanalyze: build-native
	make -C reanalyze test

test: test-analysis-binary test-reanalyze

clean: tests/node_modules/.bin/rescript
	rm -f rescript-editor-analysis.exe
	@cd tests && node_modules/.bin/rescript clean
	dune clean
	make -C reanalyze clean

.DEFAULT_GOAL := build-native

.PHONY: clean build-native dce test
