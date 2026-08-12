EMACS ?= emacs

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L lisp -l init.el -l tests/run-tests.el

compile:
	$(EMACS) -Q --batch -L lisp -l init.el \
		--eval "(setq hbmap:dir-user temporary-file-directory)" \
		--eval "(let ((byte-compile-dest-file-function \
		(lambda (file) (expand-file-name (file-name-nondirectory \
		(concat (file-name-sans-extension file) \".elc\")) \
		temporary-file-directory)))) \
		(mapc #'byte-compile-file \
		(directory-files-recursively \"lisp\" \"[.]el$$\")))"
