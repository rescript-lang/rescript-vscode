SHELL = /bin/bash

build:
	make -C document build
	make -C workspaces build

test: build
	./test.sh

clean:
	make -C document clean
	make -C workspaces clean

.DEFAULT_GOAL := test

.PHONY: clean test
