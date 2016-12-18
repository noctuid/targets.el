emacs ?= emacs

cask:
	$(shell EMACS=$(emacs) cask --verbose --debug)

test:
	@echo "Using $(shell which $(emacs))..."
	cask exec buttercup -L .

.PHONY: cask test clean
