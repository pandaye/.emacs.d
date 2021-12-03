;;; pacakge --- Summery
;;; Commentary:
;;; author: pandaye
(require 'package)

;;; Code:
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)

;; package install
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; add loadpath
(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

;; my init setting
(org-babel-load-file (expand-file-name "myinit.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
