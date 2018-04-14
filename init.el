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

(find-file "/home/keg_p/notebook.org")

;; automatically add by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
	("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
	(all-the-icons neotree py-autopep8 yasnippet-snippets magit auctex apt-utils swoop ob-ipython ox-gfm YASnippet markdown-mode graphviz-dot-mode python-mode paredit paredit-mode hlinum powerline flycheck spacemacs-theme ivy-mode ace-window which-key try use-package org-bullets)))
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
