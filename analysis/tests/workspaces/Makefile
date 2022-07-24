SHELL = /bin/bash
SUBDIRS := $(wildcard */)

build: $(SUBDIRS)
	cd $(SUBDIRS) && npm install && node_modules/.bin/rescript

clean: $(SUBDIRS)
	cd $(SUBDIRS) && rm -rf node_modules lib

.DEFAULT_GOAL := build
.PHONY: build clean