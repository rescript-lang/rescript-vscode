SHELL = /bin/bash

node_modules/.bin/rescript:
	npm install

install: node_modules/.bin/rescript

build: install
	node_modules/.bin/rescript

test: build
	./test.sh

clean:
	rm -rf node_modules lib

.DEFAULT_GOAL := build

.PHONY: build clean test install
