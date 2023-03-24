SHELL = /bin/bash

build-analysis-binary:
	rm -f rescript-editor-analysis.exe
	dune build
	cp ../_build/default/analysis/bin/main.exe rescript-editor-analysis.exe

build-tests:
	make -C tests build

build-reanalyze:
	make -C reanalyze build

build: build-analysis-binary build-reanalyze build-tests

dce: build-analysis-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

format:
	dune build @fmt --auto-promote

test-analysis-binary: build-analysis-binary
	make -C tests test

test-reanalyze: build-analysis-binary
	make -C reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	rm -f rescript-editor-analysis.exe
	dune clean
	make -C tests clean
	make -C reanalyze clean

checkformat:
	dune build @fmt

.DEFAULT_GOAL := build

.PHONY: build-analysis-binary build-reanalyze build-tests dce clean format test
