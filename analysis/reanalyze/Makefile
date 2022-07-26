SHELL = /bin/bash

install-deadcode:
	make -C examples/deadcode install

install-termination:
	make -C examples/termination install

install: install-deadcode install-termination

build-deadcode: install-deadcode
	make -C examples/deadcode build

build-termination: install-termination
	make -C examples/termination build

build: build-deadcode build-termination

test-deadcode: build-deadcode
	make -C examples/deadcode test

test-termination: build-termination
	make -C examples/termination test

test: test-deadcode test-termination

clean-deadcode:
	make -C examples/deadcode clean

clean-termination:
	make -C examples/termination clean

clean: clean-deadcode clean-termination

.DEFAULT_GOAL := build

.PHONY: build clean clean test
