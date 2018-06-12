(setq inhibit-startup-message t)
(setq column-number-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
(global-linum-mode 1)
(global-set-key (kbd "<f9>") 'eshell)
(setq-default tab-width 4)
(setq ring-bell-function 'ignore)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))

(global-set-key (kbd "<f10>") 'loop-alpha)
;当前窗口和非当前窗口时透明度
(setq alpha-list '((95 90) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))
;启动窗口时时自动开启窗口半透明效果
(loop-alpha)

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

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil))))

(defun turn-on-org-show-all-inline-images ()
  (org-display-inline-images t t))
(defun nolinum ()
  (message "Deactive linum-mode")
  (linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)

(use-package ob-ipython
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ipython . t)
   (dot . t)
   (plantuml . t)
   ))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map
                             (kbd "<f5>") 'org-revert-all-org-buffers)))

(setq org-export-with-sub-superscripts (quote {}))

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "NEXT(n)" "|" "DONE(d@/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")
              (sequence "SOMEDAY(s)"))))

;; Easy basic searches. Get a quick view of nextactions, etc
(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" todo "NEXT" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))

;; I use org's tag feature to implement contexts.
(setq org-tag-alist '(("办公" . ?s) ;; company studio office
                      ("工程项目" . ?p) ;; difference task at company
                      ("家里" . ?h) ;; home
                      ("邮件" . ?m) ;; mail somebody
                      ("乱谈" . ?l) ;; breakfast lunchtime dinner onway etc. (rest)
                      ("计算机" . ?c)
                      ("阅读" . ?r)
                      ("玄" . ?x))) ;; reading

(setq org-archive-location "%s_archive::* Archive")

(setq gtd-path (expand-file-name "~/.org-gtd"))
(defvar org-gtd-file
  (concat gtd-path "/inbox.org"))
(defun gtd ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))

(defvar org-gtd-other-files)
(setf org-gtd-other-files
      (list (concat gtd-path "/project.org")
            (concat gtd-path "/note.org")
            (concat gtd-path "/task.org")
            (concat gtd-path "/trash.org")
            (concat gtd-path "/finished.org")))
(setf org-agenda-files (cons org-gtd-file org-gtd-other-files))
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files :level . 1)))
(setq org-reverse-note-order t)  ;; note at beginning of file by default.
(setq org-default-notes-file (concat gtd-path "/inbox.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "/home/pandaye/.org-gtd/task.org" "Tasks")
         "* TODO %?\n  %i\n")
        ("i" "Idea" entry (file+headline "/home/pandaye/.org-gtd/note.org" "Idea")
         "** %?\n %T\n")
        ("j" "Journal" entry (file+datetree "/home/pandaye/.org-gtd/journal.org")
         "* %?\nEntered on %U\n  %i\n")))

;; key bingings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cg" 'gtd)
(global-set-key "\C-cc" 'org-capture)

(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-toggle)
  (global-set-key [f7] 'neotree-find)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

(unless (package-installed-p 'all-the-icons)
  (package-install all-the-icons))

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
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'cmake-mode-hook 'company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-idle-delay 0.1)
  :bind
  (("M-/" . company-complete)))

(unless (package-installed-p 'spacemacs-theme)
  (package-install spacemacs-theme))

(require 'spacemacs-common)
(deftheme spacemacs-dark "Spacemacs theme, the dark version")
(create-spacemacs-theme 'dark 'spacemacs-dark)
(provide-theme 'spacemacs-dark)
(load-theme 'spacemacs-dark t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode t))

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-python-command "python3")
  (elpy-enable)
  (pyvenv-activate "/home/pandaye/MyEnvs")
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)))

(require 'helm)
(require 'helm-config)		;?
(require 'helm-eshell)		;?
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

;; 要安装的软件包列表
(setq my-package-list
      '(rtags
        company-rtags
        company
        irony
        company-irony
        company-irony-c-headers
        flycheck-irony
        flycheck-rtags
        flycheck-irony))
;; 安装列表中尚未安装的软件包
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
          '(lambda () (setq indent-tabs-mode t)))

(require 'rtags)
(require 'company-rtags)
(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(rtags-enable-standard-keybindings)
(define-key c-mode-base-map (kbd "M-.")
  (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,")
  (function rtags-find-references-at-point))
(define-key c-mode-base-map (kbd "C-.")
  (function rtags-find-symbol))
(define-key c-mode-base-map (kbd "C-,")
  (function rtags-find-references))


(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'company-irony)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))
(setq company-show-numbers            t
      company-tooltip-limit           10
      company-dabbrev-downcase        nil)

(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package graphviz-dot-mode
  :ensure t
  :init
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantuml.jar"))
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantuml.jar"))
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
        "pandoc -f markdown -t html -s -c ~/.emacs.d/markdown/style.css --mathjax --highlight-style pygments"))

(use-package ox-gfm
  :ensure ox-gfm)

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
(use-package yasnippet-snippets
  :ensure t)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; Setting English Font
(set-face-attribute 'default nil :font "Monaco 13")

;; Chinese Font
(defun my-font-setting () 
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
              charset (font-spec :family "WenQuanyi Micro Hei"
                                 :size 26))))
(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
             (select-frame new-frame)
             (if window-system
               (my-font-setting))))
(if window-system
  (my-font-setting))

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

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))
