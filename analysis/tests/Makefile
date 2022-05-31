SHELL = /bin/bash

node_modules/.bin/rescript:
	npm install

test: node_modules/.bin/rescript
	node_modules/.bin/rescript
	./test.sh

clean: node_modules/.bin/rescript
	node_modules/.bin/rescript clean -with-deps

.DEFAULT_GOAL := test

.PHONY: clean test
