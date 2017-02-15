
INSTALL_DIR?=$(HOME)/.local/bin
export PATH := $(INSTALL_DIR):$(PATH)

ifeq ($(OS),Windows_NT)
	EXECUTABLE=$(shell stack path --dist-dir)\build\cauterize\cauterize.exe
	MKDIR=if not exist $(INSTALL_DIR) mkdir $(INSTALL_DIR)
else
	EXECUTABLE=$(shell stack path --dist-dir)/build/cauterize/cauterize
	MKDIR=mkdir -p $(INSTALL_DIR)
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
	$(MKDIR)
	cp $(EXECUTABLE) $(INSTALL_DIR)
