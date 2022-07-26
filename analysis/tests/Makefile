SHELL = /bin/bash

buildDocument:
	make -C document build

buildWorkspaceSymbols:
	make -C workspaceSymbols build

build: buildDocument buildWorkspaceSymbols

test: build
	./test.sh

install-document:
	make -C document install

install-workspaceSymbols:
	make -C workspaceSymbols install

install: install-document install-workspaceSymbols

clean:
	make -C document clean
	make -C workspaceSymbols clean

.DEFAULT_GOAL := test

.PHONY: clean test
