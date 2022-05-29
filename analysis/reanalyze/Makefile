SHELL = /bin/bash

test:
	make -C examples/deadcode test
	make -C examples/termination test

clean:
	make -C examples/deadcode clean
	make -C examples/termination clean

.DEFAULT_GOAL := test

.PHONY: clean test
