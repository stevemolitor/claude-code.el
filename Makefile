EMACS ?= emacs

# Run all tests by default.
MATCH ?=

.PHONY: test clean

default: all

# Remove compiled files
clean:
	rm -f *.elc

EL_FILES := $(wildcard *.el)

# Run checkdoc on elisp files. To do this, we run checkdoc-file via -eval on every .el file in EL_FILES
checkdoc:
	for FILE in ${EL_FILES}; do $(EMACS) --batch -L . -eval "(setq sentence-end-double-space nil)" -eval "(checkdoc-file \"$$FILE\")" ; done

compile: clean
	$(EMACS) --batch -L . --eval "(progn (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (package-initialize) (setq sentence-end-double-space nil))" -f batch-byte-compile *.el

all: checkdoc compile
