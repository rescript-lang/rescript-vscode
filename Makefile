SHELL = /bin/bash

build:
	make -C analysis build

clean:
	make -C analysis clean

test:
	make -C analysis test

.DEFAULT_GOAL := build

.PHONY: build clean test
