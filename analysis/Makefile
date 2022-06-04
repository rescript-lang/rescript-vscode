SHELL = /bin/bash

build-native:
	rm -f rescript-editor-analysis.exe
	dune build
	cp _build/install/default/bin/rescript-editor-analysis rescript-editor-analysis.exe

dce: build-native
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

test-analysis-binary: build-native
	make -C tests test

test-reanalyze: build-native
	make -C reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	rm -f rescript-editor-analysis.exe
	dune clean
	make -C reanalyze clean

.DEFAULT_GOAL := build-native

.PHONY: clean build-native dce test
