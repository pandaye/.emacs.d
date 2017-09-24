;;; pacakge --- Summary
;;; Commentary:
;;; author: pandaye
(require 'package)
;;; Code:
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.emacs-china.org/gnu/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("marmalade" . "https://elpa.emacs-china.org/marmalade/")))
(package-initialize)

;; package install
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; add loadpath
(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

;; my init setting
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(find-file "/home/pandaye/Notes/Study/homework.org")

;; automatically add by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work
 '(custom-safe-themes
   (quote
	("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
	(ox-gfm YASnippet markdown-mode graphviz-dot-mode python-mode paredit paredit-mode hlinum powerline flycheck spacemacs-theme auto-complete ivy-mode ace-window which-key try use-package org-bullets)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(flymake-errline ((t nil)))
 '(flymake-warnline ((t nil))))

(provide 'init)
;;; init.el ends here
