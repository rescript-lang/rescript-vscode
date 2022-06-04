SHELL = /bin/bash

node_modules/.bin/rescript:
	npm install

build: node_modules/.bin/rescript
	node_modules/.bin/rescript

test: build node_modules/.bin/rescript
	./test.sh

clean: node_modules/.bin/rescript
	node_modules/.bin/rescript clean -with-deps

clean-deep:
	rm -r node_modules lib

.DEFAULT_GOAL := build

.PHONY: build clean clean-deep test
