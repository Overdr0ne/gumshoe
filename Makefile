emacs ?= emacs

LOAD = -l elpa.el -l gumshoe.el
RM ?= rm -f

# Core module files to compile (no external deps)
CORE_SOURCES = context.el etree.el gumshoe-lib.el gumshoe-backtracker.el \
               gumshoe-footprints.el gumshoe-peruse.el gumshoe-ring.el \
               gumshoe.el

# Optional module files (may have external dependencies)
OPTIONAL_SOURCES = gumshoe-tree.el

all: test-load compile-test install-test test

deps:
	$(emacs) -batch -l targets/install-deps.el

test-load:
	@echo "Testing that all modules load without errors..."
	$(emacs) -batch -l elpa.el \
		--eval "(progn \
		  (add-to-list 'load-path default-directory) \
		  (require 'context) \
		  (require 'etree) \
		  (require 'gumshoe-lib) \
		  (require 'gumshoe-backtracker) \
		  (require 'gumshoe-footprints) \
		  (require 'gumshoe-peruse) \
		  (require 'gumshoe-ring) \
		  (require 'gumshoe-tree) \
		  (require 'gumshoe) \
		  (message \"All modules loaded successfully\"))"

test:
	@echo "Running ERT tests..."
	$(emacs) -batch $(LOAD) -l gumshoe-test.el -f gumshoe-test-run-tests

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	@echo "Byte-compiling core source files..."
	$(emacs) -batch -l elpa.el -L . -f batch-byte-compile $(CORE_SOURCES)
	@echo "Attempting to compile optional modules (may fail if deps missing)..."
	-@$(emacs) -batch -l elpa.el -L . -f batch-byte-compile $(OPTIONAL_SOURCES) 2>/dev/null || true

compile-test:
	@echo "Testing that core files compile without errors..."
	@if $(emacs) -Q --batch -L . -f batch-byte-compile $(CORE_SOURCES) 2>&1 | grep -iE "(error|warning)"; then \
		echo "FAIL: Compilation errors or warnings found"; \
		exit 1; \
	else \
		echo "PASS: All core files compile cleanly"; \
	fi

install-test:
	@echo "Testing package installation (simulates MELPA)..."
	@echo "This reproduces the exact scenario from issue #22"
	@$(emacs) -Q --batch -l test-package-install.el

plain:
	$(emacs) --version
	$(emacs) -Q -l elpa.el

clean:
	$(RM) *.elc

.PHONY: all test test-load checkdoc compile compile-test install-test plain deps clean
