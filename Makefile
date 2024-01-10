SHELL = /bin/bash

build:
	dune build
	cp _build/install/default/bin/rescript-editor-analysis rescript-editor-analysis.exe
	cp _build/install/default/bin/rescript-tools rescript-tools.exe

test:
	make -C analysis test
	make -C tools/tests test

clean:
	dune clean
	make -C analysis clean
	make -C tools/tests clean

format:
	dune build @fmt --auto-promote

checkformat:
	dune build @fmt

.DEFAULT_GOAL := build

.PHONY: build clean test
