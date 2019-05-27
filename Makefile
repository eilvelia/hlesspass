.PHONY: default
default: install

.PHONY: build
build:
	stack build

.PHONY: install
install:
	stack install

.PHONY: test
test:
	stack test
