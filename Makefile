
INSTALL_DIR?=$(HOME)/.local/bin
export PATH := $(INSTALL_DIR):$(PATH)

.PHONY: default clean build test

default: test

clean:
	-@rm -rf .stack-work

build:
	@stack build

test:
	@stack test

install:
	stack build
	mkdir -p $(INSTALL_DIR)
	cp `stack path --dist-dir`/build/cauterize/cauterize \
		$(INSTALL_DIR)/cauterize
