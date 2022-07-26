SHELL = /bin/bash

buildDocument:
	make -C document build

buildWorkspaces:
	make -C workspaces/workspaceSymbols build

build: buildDocument buildWorkspaces

test: build
	./test.sh

clean:
	make -C document clean
	make -C workspaces/workspaceSymbols clean

.DEFAULT_GOAL := test

.PHONY: clean test
