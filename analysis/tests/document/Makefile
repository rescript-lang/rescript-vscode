SHELL = /bin/bash

node_modules/.bin/rescript:
	npm install

build: node_modules/.bin/rescript
	node_modules/.bin/rescript

clean:
	rm -r node_modules lib

.PHONY: clean