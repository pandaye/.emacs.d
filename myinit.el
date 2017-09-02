(setq inhibit-startup-message t)
(setq column-number-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 3)
  (smooth-scrolling-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme .t)
   ))

(defalias 'list-buffers 'ibuffer)

(winner-mode 1)
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    (setq ac-auto-start nil)
    (setq ac-quick-help-delay 0.2)
    (define-key ac-mode-map (kbd "M-/") 'auto-complete)))

(use-package spacemacs-theme
  :ensure t
  :init
  (progn
    (require 'spacemacs-common)
    (deftheme spacemacs-dark "Spacemacs theme, the dark version")
    (create-spacemacs-theme 'dark 'spacemacs-dark)
    (provide-theme 'spacemacs-dark)
    (load-theme 'spacemacs-dark t)
    ))

(use-package ox-reveal
  :ensure ox-reveal)
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:complete-on-dot t))

(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)))

(require 'helm)
(require 'helm-config)			;?
(require 'helm-eshell)			;?
(require 'helm-files)			;?
(require 'helm-grep)

; do not display invisible candidates
(setq helm-quick-update t)
; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)
; fuzzy matching buffer names when non--nil
(setq helm-buffers-fuzzy-matching t)
; move to end or beginning of source when reaching top or bottom of source.
(setq helm-move-to-line-cycle-in-source nil)
; search for library in `require' and `declare-function' sexp.
(setq helm-ff-search-library-in-sexp t)
; scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount 8)
(setq helm-ff-file-name-history-use-recentf t)

(use-package helm-swoop
  :ensure t
  :bind (("C-s" . helm-swoop)
         ("C-r" . helm-swoop)))

(helm-mode 1)

(require 'myscheme)

(use-package graphviz-dot-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init 
  (setq markdown-command
        "pandoc -f markdown -t html -s -c /home/pandaye/CSS/style.css --mathjax --highlight-style pygments"))

(use-package ox-gfm
  :ensure ox-gfm)

(use-package auto-complete-clang
  :ensure t
  :init
  (setq ac-clang-flags
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
 /usr/include/c++/7.1.1
 /usr/include/c++/7.1.1/x86_64-pc-linux-gnu
 /usr/include/c++/7.1.1/backward
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include
 /usr/local/include
 /usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include-fixed
 /usr/include
"
                 )))
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))

(require 'auto-complete-clang)
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(use-package auto-complete-c-headers
  :ensure t)
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1")
  (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1/x86_64-redhat-linux")
  (add-to-list 'achead:include-directories '"/usr/include/c++/7.1.1/backward")
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-redhat-linux/7.1.1/include")
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-redhat-linux/7.1.1/include-fixed")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  (add-to-list 'achead:include-directories '"/usr/include")
  )
(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

(setq c-default-style "linux"
  c-basic-offset 4
  indent-tabs-mode t)
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map [C-tab] 'yas-expand))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
