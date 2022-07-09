SHELL = /bin/bash

build:
	make -C analysis build

clean:
	make -C analysis clean

test:
	make -C analysis test

format:
	make -C analysis format

checkformat:
	make -C analysis checkformat

.DEFAULT_GOAL := build

.PHONY: build clean test
