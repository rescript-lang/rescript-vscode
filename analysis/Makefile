SHELL = /bin/bash

build-native:
	dune build

dce:
	dune build @check @all
	../../reanalyze/_build/install/default/bin/reanalyze.exe -dce-cmt _build -suppress src/vendor

tests/node_modules/.bin/rescript:
	@cd tests && npm install

test: build-native tests/node_modules/.bin/rescript
	@cd tests && node_modules/.bin/rescript
	./test.sh

clean: tests/node_modules/.bin/rescript
	@cd tests && node_modules/.bin/rescript clean
	dune clean

.DEFAULT_GOAL := build-native

.PHONY: clean build-native dce test
