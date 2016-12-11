
INSTALL_DIR?=$(HOME)/.local/bin
export PATH := $(INSTALL_DIR):$(PATH)

ifeq ($(OS),Windows_NT)
	EXECUTABLE=$(shell stack path --dist-dir)\build\cauterize\cauterize.exe
else
	EXECUTABLE=$(shell stack path --dist-dir)/build/cauterize/cauterize
endif

.PHONY: default clean build test

default: test

clean:
	-@rm -rf .stack-work

build:
	@stack build

test:
	@stack test

install:
	stack setup
	stack build
	mkdir -p $(INSTALL_DIR)
	cp $(EXECUTABLE) $(INSTALL_DIR)
