SHELL = /bin/bash

build: node_modules/.bin/rescript
	node_modules/.bin/rescript

node_modules/.bin/rescript:
	npm install

clean:
	rm -rf node_modules lib

.PHONY: clean build