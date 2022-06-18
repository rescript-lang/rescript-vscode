SHELL = /bin/bash

build:
	make -C analysis build

clean:
	make -C analysis clean

clean-deep:
	make -C analysis clean-deep

test:
	make -C analysis test

.DEFAULT_GOAL := build

.PHONY: build clean clean-deep test
