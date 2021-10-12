(setq inhibit-startup-message t)
(setq column-number-mode t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
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

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(global-set-key (kbd "<f10>") 'loop-alpha)
;当前窗口和非当前窗口时透明度
(setq alpha-list '((100 100) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))
;;启动窗口时时自动开启窗口半透明效果
;; (loop-alpha)

(set-face-attribute 'fringe nil :background nil)

(define-fringe-bitmap 'left-arrow [])
(define-fringe-bitmap 'left-curly-arrow [])
(define-fringe-bitmap 'left-triangle [])

(defface fallback '((t :family "Fira Code Light"
                       :foreground "gray")) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Setting English Font

(set-face-attribute 'default nil :font "Fira Mono 13")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Noto Sans CJK SC")))
; (setq face-font-rescale-alist '(("Noto Sans CJK SC" . 1.2)))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; (use-package smooth-scrolling
;;   :ensure t
;;   :config
;;   (setq smooth-scroll-margin 3)
;;   (smooth-scrolling-mode 1))
(setq scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-margin 3
      scroll-conservatively 5
      redisplay-skip-fontification-on-input t)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
)

(defun turn-on-org-show-all-inline-images ()
  (org-display-inline-images t t))

(add-hook 'org-mode-hook
      (lambda()
        (setq truncate-lines nil))) 
(add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)

(use-package ob-ipython
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (scheme . t)
   (dot . t)
   (plantuml . t)
   ))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map
                             (kbd "<f5>") 'org-revert-all-org-buffers)))

(setq org-export-with-sub-superscripts (quote {}))
(setq org-src-fontify-natively t)

(setq org-babel-python-command "python3")

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))

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
(setq org-tag-alist '(("@Study" . ?s) ;; company studio office
                      ("@Project" . ?p) ;; difference task at company
                      ("@Life" . ?l) ;; home
                      ("@Mail" . ?m) ;; mail somebody
                      ("@Record" . ?r) ;; breakfast lunchtime dinner onway etc. (rest)
                      ("@Note" . ?n)
                      ("@Question" . ?q))) ;; quastion


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
            (concat gtd-path "/finished.org")
            (concat gtd-path "/record.org")))
(setf org-agenda-files (cons org-gtd-file org-gtd-other-files))
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files :level . 1)))
(setq org-reverse-note-order t)  ;; note at beginning of file by default.
(setq org-default-notes-file (concat gtd-path "/inbox.org"))
(setq todofile (concat gtd-path "/task.org"))
(setq notefile (concat gtd-path "/note.org"))
(setq journalfile (concat gtd-path "/journal.org"))
(setq orgarchive (concat gtd-path "/archive.org"))
(setq org-archive-location (concat orgarchive "::* Archive"))
(setq org-capture-templates
      '(("t" "Todo" entry (file todofile)
         "* TODO %?\nSCHEDULED: %t\n")
        ("i" "Idea" entry (file+headline notefile "Idea")
         "** %?\n %T\n")
        ("j" "Journal" entry (file+datetree journalfile)
         "* %?\nEntered on %U\n  %i\n")
        ("w" "Web" entry (file+headline "" "Web")
         "** %U %^{Title}\n%x")))

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
  (package-install 'all-the-icons)
  (all-the-icons-install-fonts t)
  )

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

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)))

(require 'helm)
(require 'helm-config)      ;?
(require 'helm-eshell)      ;?
(require 'helm-files)       ;?
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

(use-package helm-xref
  :ensure t
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(helm-mode 1)

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-idle-delay 0.1)
  :bind
  (("M-/" . company-complete)))

(use-package lsp-mode
  :ensure t
  :commands lsp)
;(use-package company-lsp
;  :ensure t
;  :commands company-lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  ((lsp-mode . lsp-ui-mode))
  :bind
  ("s-i" . lsp-ui-imenu))

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
          '(lambda () (setq indent-tabs-mode t)))

(require 'myscheme)
(use-package racket-mode
  :ensure t
  :config
  (setq racket-racket-program "racket")
  (setq racket-raco-program "raco")
  :bind
  (:map racket-mode-map
        ("C-x C-j" . racket-run)))

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

(use-package yaml-mode
  :ensure t)

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

(use-package auctex
   :defer t
   :ensure auctex
   :init
   (require 'advance-words-count)
   (setq TeX-auto-save t)
   (setq TeX-parse-self t)
   (setq-default TeX-master nil)
   (add-hook 'LaTeX-mode-hook
             (lambda ()
               (turn-on-auto-fill)
               (turn-on-reftex)
               (LaTeX-math-mode 1)
               (setq TeX-show-complilation nil)
               (setq TeX-clean-confirm nil)
               (setq TeX-save-query nil)
               (setq TeX-view-program-list '(("Okular" "okular %o")))
               (setq TeX-view-program-selection
                     '((output-pdf "Okular")))
               (setq TeX-engine 'xetex)
               (TeX-global-PDF-mode t)
               (add-to-list 'TeX-command-list
                             '("XeLaTeX" "%'xelatex%(mode)%' %t"
                                          TeX-run-TeX nil t))
               (setq TeX-command-default "XeLaTeX"))
   )
   :config
   (setq TeX-fold-env-spec-list
         (quote (("[figure]" ("figure"))
                 ("[table]" ("table"))
                 ("[itemize]" ("itemize"))
                 ("[overpic]" ("overpic")))))
 )

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  ;; Highlight changes on editing.
  (diff-hl-flydiff-mode)
  ;; Makes fringe and margin react to mouse clicks to show the curresponding hunk.
  (diff-hl-show-hunk-mouse-mode)
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#e9cd43"))))
  (diff-hl-insert ((t (:background "#03e94f"))))
  (diff-hl-delete ((t (:background "#f5597e")))))

;; wanderlust
(unless (package-installed-p 'wanderlust)
  (package-install 'wanderlust))
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(require 'wl-spam)
