emacs ?= emacs

LOAD = -l elpa.el -l gumshoe.el
RM ?= rm -f

all: test

deps:
	$(emacs) -batch -l targets/install-deps.el

test:
	$(emacs) -batch $(LOAD) -l gumshoe-test.el -f gumshoe-test-run-tests

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch -l elpa.el -L . -f batch-byte-compile gumshoe.el

plain:
	$(emacs) --version
	$(emacs) -Q -l elpa.el

clean:
	$(RM) *.elc

.PHONY: all test checkdoc compile plain update-issues clean
