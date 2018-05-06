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

;; automatically add by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
	(rtags htmlize company-rtags org irony company-irony company-irony-c-headers helm-bind-key paredit use-package powerline hlinum try which-key smooth-scrolling rainbow-delimiters org-bullets ob-ipython neotree all-the-icons ace-window company spacemacs-theme flycheck elpy py-autopep8 helm helm-swoop graphviz-dot-mode plantuml-mode flycheck-plantuml markdown-mode ox-gfm company-c-headers yasnippet yasnippet-snippets auctex magit)))
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
