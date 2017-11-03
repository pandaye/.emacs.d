(setq inhibit-startup-message t)
(setq column-number-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
(global-set-key (kbd "<f9>") 'eshell)
(setq-default tab-width 4)

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

(defun turn-on-org-show-all-inline-images ()
  (org-display-inline-images t t))

(add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)

(use-package ob-ipython
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ipython . t)
   (dot . t)
   ))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "DarkOrange" :weight bold))
        ("DOING" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold)) 
        ("CLOSED" . (:foreground "red"))
        ))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map
                             (kbd "<f5>") 'org-revert-all-org-buffers)))

(setq org-export-with-sub-superscripts (quote {}))

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

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.16)
  :bind
  (("M-/" . company-complete)))

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

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (pyvenv-activate "/home/pandaye/MyEnvs")
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (add-hook 'elpy-mode-hook 'company-mode))

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

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path "/home/pandaye/.emacs.d/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
(use-package flycheck-plantuml
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; 配置输出指令
  (setq markdown-command
        "pandoc -f markdown -t html -s -c /home/pandaye/CSS/style.css --mathjax --highlight-style pygments"))

(use-package ox-gfm
  :ensure ox-gfm)

(require 'company)
(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/7.1.1/"))

(setq c-default-style "linux"
      c-basic-offset 4)
;; (add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))
(add-hook 'c-mode-common-hook '(lambda () (setq indent-tabs-mode t)))
(add-hook 'c-mode-common-hook 'company-mode)

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
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Setting English Font
(set-face-attribute 'default nil :font "DejaVu Sans Mono 13")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
            charset (font-spec :family "WenQuanyi MicroHei"
                       :size 26)))

(use-package auctex
  :defer t
  :ensure auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-auto-fill)
              (LaTeX-math-mode 1)
              (setq TeX-show-complilation nil)
              (setq TeX-clean-confirm nil)
              (setq TeX-save-query nil)
              (setq TeX-view-program-list '(("Evince" "evince %o")))
              (setq TeX-view-program-selection
                    '((output-pdf "Evince")))
              (setq TeX-engine 'xetex)
              (TeX-global-PDF-mode t)
              (add-to-list 'TeX-command-list
                            '("XeLaTeX" "%'xelatex%(mode)%' %t"
                                         TeX-run-TeX nil t))
              (setq TeX-command-default "XeLaTeX"))
  )
)
