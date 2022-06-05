SHELL = /bin/bash

build-analysis-binary:
	rm -f rescript-editor-analysis.exe
	dune build
	cp _build/install/default/bin/rescript-editor-analysis rescript-editor-analysis.exe

build-tests:
	make -C tests build

build-reanalyze:
	make -C reanalyze build

build: build-analysis-binary build-reanalyze build-tests

dce: build-analysis-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

test-analysis-binary: build-analysis-binary
	make -C tests test

test-reanalyze: build-analysis-binary
	make -C reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	rm -f rescript-editor-analysis.exe
	dune clean
	make -C reanalyze clean

clean-deep:
	make clean
	make -C reanalyze clean-deep

.DEFAULT_GOAL := build

.PHONY: build-analysis-binary build-reanalyze build-tests dce clean clean-deep test
