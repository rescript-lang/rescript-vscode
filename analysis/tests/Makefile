SHELL = /bin/bash

buildDocument:
	make -C document build

buildWorkspaceSymbols:
	make -C workspaceSymbols build

build: buildDocument buildWorkspaceSymbols

test: build
	./test.sh

clean:
	make -C document clean
	make -C workspaceSymbols clean

.DEFAULT_GOAL := test

.PHONY: clean test
