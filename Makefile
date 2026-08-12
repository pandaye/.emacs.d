EMACS ?= emacs

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L lisp -l init.el -l tests/run-tests.el

compile:
	$(EMACS) -Q --batch -L lisp -f batch-byte-compile $$(find lisp -maxdepth 1 -name 'my-*.el' -print)
