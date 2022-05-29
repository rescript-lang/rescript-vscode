SHELL = /bin/bash

build-native:
	rm -f rescript-editor-analysis.exe
	dune build
	cp _build/install/default/bin/rescript-editor-analysis.exe rescript-editor-analysis.exe

dce:
	dune build
	../../reanalyze/_build/install/default/bin/reanalyze.exe -dce-cmt _build -suppress vendor

tests/node_modules/.bin/rescript:
	@cd tests && npm install

test: build-native tests/node_modules/.bin/rescript
	@cd tests && node_modules/.bin/rescript
	./test.sh

test-reanalyze: build-native
	@cd reanalyze && node scripts/run_integration_tests.js

test-all: test test-reanalyze

clean: tests/node_modules/.bin/rescript
	rm -f rescript-editor-analysis.exe
	@cd tests && node_modules/.bin/rescript clean
	dune clean

.DEFAULT_GOAL := build-native

.PHONY: clean build-native dce test
