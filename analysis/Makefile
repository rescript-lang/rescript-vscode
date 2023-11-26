SHELL = /bin/bash

build-tests:
	make -C tests build

build-reanalyze:
	make -C reanalyze build

build: build-reanalyze build-tests

dce: build-analysis-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

test-analysis-binary:
	make -C tests test

test-reanalyze:
	make -C reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	make -C tests clean
	make -C reanalyze clean

.PHONY: build-reanalyze build-tests dce clean test
