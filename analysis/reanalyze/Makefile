SHELL = /bin/bash

test:
	make -C examples/deadcode test
	make -C examples/termination test

clean:
	make -C examples/deadcode clean
	make -C examples/termination clean

clean-deep:
	make -C examples/deadcode clean-deep
	make -C examples/termination clean-deep

.DEFAULT_GOAL := test

.PHONY: clean clean-deep test
