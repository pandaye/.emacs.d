EMACS ?= emacs
# 测试要加载“本目录”的配置，而不是 ~/.emacs.d；否则在 git worktree 里
# 跑 make test 实际测的是主工作区的 lisp/。
ROOT := $(CURDIR)

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L lisp \
		--eval "(setq user-emacs-directory (file-name-as-directory \"$(ROOT)\"))" \
		-l init.el -l tests/run-tests.el

compile:
	$(EMACS) -Q --batch -L lisp -L lisp/core -L lisp/modules -L lisp/features \
		--eval "(progn (require 'package) (package-initialize) \
		(require 'use-package) (setq use-package-always-ensure nil))" \
		--eval "(setq hbmap:dir-user temporary-file-directory)" \
		--eval "(let ((byte-compile-dest-file-function \
		(lambda (file) (expand-file-name (file-name-nondirectory \
		(concat (file-name-sans-extension file) \".elc\")) \
		temporary-file-directory)))) \
		(mapc #'byte-compile-file \
		(directory-files-recursively \"lisp\" \"[.]el$$\")))"
